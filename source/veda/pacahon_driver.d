module veda.pacahon_driver;

import std.stdio, std.datetime, std.conv, std.string;
import vibe.core.concurrency, vibe.core.core, vibe.core.log, vibe.core.task;

import pacahon.server;
import pacahon.context;
import pacahon.thread_context;
import onto.owl;
import onto.individual;
import onto.resource;
import onto.lang;
import util.lmultidigraph;

/*
 * TODO: Проблема, не удается передавать структуру типа Individual между волокнами, хотя например Tid, хорошо передается
 *	 приходится передавать так  - immutable(Individual)[].
 *	 Вероятно это связанно с ассоциативными массивами внутри Individual.
 */

enum Command {
    Get,
    Is,
    Put
}

enum Function {
    AllClasses,
    Class,
    Individual,
    PropertyOfIndividual,
    IndividualsToQuery,
    NewTicket,
    TicketValid
}

Task io_task;

class PacahonDriver {
    Context context;
    this() {
        init_core();
        context = new ThreadContext(props_file_path, "vibe.app");
    }

    void init() {
        writeln("START VEDA STORAGE FIBER LISTENER");
        io_task = runTask({
                              immutable(Individual) _empty_iIndividual = (immutable(Individual)).init;
                              immutable(Class) _empty_iClass = (immutable(Class)).init;
                              Resources _empty_Resources = Resources.init;

                              while (true) {
                                  receive(
                                          (Command cmd, Function fn, string arg1, byte arg2, string ticket, Tid tid) {
                                              // writeln("Tid=", cast(void *)tid);
                                              if (tid !is null) {
                                                  if (cmd == Command.Get && fn == Function.Individual) {
                                                      immutable(Individual)[ string ] onto_individuals =
                                                          context.get_onto_as_map_individuals();
                                                      immutable(Individual)[] individuals;

                                                      immutable(Individual) individual = onto_individuals.get(arg1, _empty_iIndividual);

                                                      if (individual != _empty_iIndividual)
                                                          individuals ~= individual;
                                                      else
                                                          individuals ~= context.get_individual(arg1, ticket, arg2).idup;

                                                      send(tid, individuals);
                                                  } else if (cmd == Command.Get && fn == Function.IndividualsToQuery) {
                                                      immutable(Individual)[] individuals =
                                                          context.get_individuals_via_query(arg1, ticket, arg2);

                                                      send(tid, individuals);
                                                  }
                                              }
                                          },
                                          (Command cmd, Function fn, string args, Tid tid) {
                                              // writeln("Tid=", cast(void *)tid);
                                              if (tid !is null) {
                                                  if (cmd == Command.Get && fn == Function.AllClasses) {
                                                      send(tid, context.get_owl_classes().values);
                                                  } else if (cmd == Command.Is && fn == Function.TicketValid) {
                                                      bool res = context.is_ticket_valid(args);
                                                      send(tid, res);
                                                  } else if (cmd == Command.Get && fn == Function.Class) {
                                                      immutable(Class)[] classes;
                                                      Ticket ticket;

                                                      immutable(Class) classz = context.get_owl_classes().get(args, _empty_iClass);

                                                      if (classz != _empty_iClass)
                                                          classes ~= classz;

                                                      send(tid, classes);
                                                  }
                                              }
                                          },
                                          (Command cmd, Function fn, string arg1, string arg2, Tid tid) {
                                              if (tid !is null) {
                                                  if (cmd == Command.Get && fn == Function.NewTicket) {
                                                      immutable(Ticket)[] tickets;
                                                      Ticket ticket = context.authenticate(arg1, arg2);
                                                      tickets ~= ticket;

                                                      send(tid, tickets.idup);
                                                  }
                                              }
                                          },
                                          (Command cmd, Function fn, string ticket, string uri, immutable(Individual)[] individual, Tid tid) {
                                              if (tid !is null) {
                                                  if (cmd == Command.Put && fn == Function.Individual) {
                                                      ResultCode res = context.put_individual(uri, cast(Individual)individual[ 0 ], ticket);

                                                      send(tid, res);
                                                  }
                                              }
                                          },
                                          (Command cmd, Function fn, string arg1, string arg2, string ticket, LANG lang, Tid tid) {
                                              if (tid !is null) {
                                                  if (cmd == Command.Get && fn == Function.PropertyOfIndividual) {
                                                      string res1;
                                                      immutable(Individual)[ string ] onto_individuals =
                                                          context.get_onto_as_map_individuals();
                                                      immutable(Individual) individual = onto_individuals.get(arg1, _empty_iIndividual);
                                                      if (individual == _empty_iIndividual) {
                                                          immutable(Individual) individual1 =
                                                              context.get_individual(arg1, ticket).idup;
                                                          immutable Resources res = individual1.resources.get(arg2, Resources.init);

                                                          foreach (rr; res) {
                                                              if (rr.lang == lang) {
                                                                  res1 = rr.data;
                                                                  break;
                                                              }
                                                          }
                                                      } else {
                                                          immutable Resources res = individual.resources.get(arg2, Resources.init);

                                                          foreach (rr; res) {
                                                              if (rr.lang == lang) {
                                                                  res1 = rr.data;
                                                                  break;
                                                              }
                                                          }
                                                      }
                                                      send(tid, res1);
                                                  }
                                              }
                                          },
                                          (int msg, Tid tid) {
                                              logInfo("Received int message: %s", msg);
                                          });
                              }
                          });
    }
}