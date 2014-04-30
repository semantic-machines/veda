module veda.pacahon_driver;

import std.stdio, std.datetime, std.conv, std.string, std.variant;;
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

enum Command
{
    Get     = 1,
    Is      = 2,
    Put     = 3,
    Set     = 3,
    Execute = 4,
    Wait    = 5
}

enum Function
{
    AllClasses,
    Class,
    Individual,
    Individuals,
    PropertyOfIndividual,
    IndividualsToQuery,
    IndividualsIdsToQuery,
    NewTicket,
    TicketValid,
    Script,
    PModule,
    Trace,
    Backup
}

Task io_task;

class PacahonDriver {
    Context context;
    this() {
        init_core();
        context = new PThreadContext(props_file_path, "vibe.app");
    }

    void init()
    {
        writeln("START VEDA STORAGE FIBER LISTENER");
        io_task = runTask({
                              immutable(Individual) _empty_iIndividual = (immutable(Individual)).init;
                              immutable(Class) _empty_iClass = (immutable(Class)).init;
                              Resources _empty_Resources = Resources.init;

                              while (true)
                              {
                                  receive(
                                          (Command cmd, Function fn, Tid tid)
                                          {
                                              if (tid != Tid.init)
                                              {
                                                  if (cmd == Command.Execute && fn == Function.Backup)
                                                  {
                                                      context.backup();
                                                      send(tid, true);
                                                  }
                                              }
                                          },
                                          (Command cmd, Function fn, int arg1, bool arg2, Tid tid)
                                          {
                                              if (tid != Tid.init)
                                              {
                                                  if (cmd == Command.Set && fn == Function.Trace)
                                                  {
                                                      context.set_trace(arg1, arg2);
                                                      send(tid, true);
                                                  }
                                              }
                                          },
                                          (Command cmd, Function fn, P_MODULE thread_id, Tid tid)
                                          {
                                              if (tid != Tid.init)
                                              {
                                                  if (cmd == Command.Wait && fn == Function.PModule)
                                                  {
                                                      context.wait_thread(thread_id);
                                                      send(tid, true);
                                                  }
                                              }
                                          },
                                          (Command cmd, Function fn, immutable(string)[] arg1, string arg2, Tid tid)
                                          {
                                              if (tid != Tid.init)
                                              {
                                                  if (cmd == Command.Get && fn == Function.Individuals)
                                                  {
                                                      immutable(Individual)[] individuals;
                                                      Ticket *ticket = context.get_ticket(arg2);
                                                      if (ticket.result == ResultCode.OK)
                                                      {
                                                          foreach (indv; context.get_individuals(ticket, arg1.dup))
                                                              individuals ~= indv.idup;
                                                      }
                                                      send(tid, individuals, ticket.result);
                                                  }
                                              }
                                          },
                                          // writeln("Tid=", cast(void *)tid);
                                          (Command cmd, Function fn, string arg1, string arg2, Tid tid) {
                                          // writeln("Tid=", cast(void *)tid);
                                              if (tid != Tid.init)
                                              {
                                                  if (cmd == Command.Get && fn == Function.NewTicket)
                                                  {
                                                      immutable(Ticket)[] tickets;
                                                      Ticket ticket = context.authenticate(arg1, arg2);
                                                      tickets ~= ticket;

                                                      send(tid, tickets.idup);
                                                  }
                                                  else if (cmd == Command.Get && fn == Function.Individual)
                                                  {
                                                      immutable(Individual)[ string ] onto_individuals =
                                                          context.get_onto_as_map_individuals();
                                                      immutable(Individual)[] individuals;

                                                      immutable(Individual) individual = onto_individuals.get(arg1, _empty_iIndividual);

                                                      ResultCode rc;

                                                      if (individual != _empty_iIndividual)
                                                      {
                                                          rc = ResultCode.OK;
                                                          individuals ~= individual;
                                                      }
                                                      else
                                                      {
                                                          Ticket *ticket = context.get_ticket(arg2);
                                                          rc = ticket.result;
                                                          if (rc == ResultCode.OK)
                                                          {
                                                              individuals ~= context.get_individual(ticket, arg1).idup;
                                                          }
                                                      }

                                                      send(tid, individuals, rc);
                                                  }
                                                  else if (cmd == Command.Get && fn == Function.IndividualsToQuery)
                                                  {
                                                      Ticket *ticket = context.get_ticket(arg2);

                                                      if (ticket.result == ResultCode.OK)
                                                      {
                                                          immutable(Individual)[] individuals =
                                                              context.get_individuals_via_query(ticket, arg1);
                                                          send(tid, individuals, ticket.result);
                                                      }
                                                      else
                                                      {
                                                          immutable(Individual)[] individuals;
                                                          send(tid, individuals, ticket.result);
                                                      }
                                                  }
                                                  else if (cmd == Command.Get && fn == Function.IndividualsIdsToQuery)
                                                  {
                                                      Ticket *ticket = context.get_ticket(arg2);

                                                      if (ticket.result == ResultCode.OK)
                                                      {
                                                          immutable(string)[] uris = context.get_individuals_ids_via_query(ticket, arg1);
                                                          send(tid, uris, ticket.result);
                                                      }
                                                      else
                                                      {
                                                          immutable(string)[] uris;
                                                          send(tid, uris, ticket.result);
                                                      }
                                                  }
                                              }
                                          },
                                          (Command cmd, Function fn, string args, Tid tid) {
                                              // writeln("Tid=", cast(void *)tid);
                                              if (tid !is Tid.init)
                                              {
                                                  if (cmd == Command.Execute && fn == Function.Script)
                                                  {
                                                      send(tid, context.execute_script(args));
                                                  }
                                                  else if (cmd == Command.Get && fn == Function.AllClasses)
                                                  {
                                                      send(tid, context.get_owl_classes().values);
                                                  }
                                                  else if (cmd == Command.Is && fn == Function.TicketValid)
                                                  {
                                                      bool res = context.is_ticket_valid(args);
                                                      send(tid, res);
                                                  }
                                                  else if (cmd == Command.Get && fn == Function.Class)
                                                  {
                                                      immutable(Class)[] classes;
                                                      Ticket ticket;

                                                      immutable(Class) classz = context.get_owl_classes().get(args, _empty_iClass);

                                                      if (classz != _empty_iClass)
                                                          classes ~= classz;

                                                      send(tid, classes);
                                                  }
                                              }
                                          },
                                          (Command cmd, Function fn, string _ticket, string uri, immutable(Individual)[] individual,
                                           Tid tid)
                                          {
                                              if (tid !is Tid.init)
                                              {
                                                  if (cmd == Command.Put && fn == Function.Individual)
                                                  {
                                                      Ticket *ticket = context.get_ticket(_ticket);
                                                      if (ticket.result == ResultCode.OK)
                                                      {
                                                          ResultCode res =
                                                              context.put_individual(ticket, uri, cast(Individual)individual[ 0 ]);
                                                          send(tid, res);
                                                      }
                                                      else
                                                      {
                                                          send(tid, ticket.result);
                                                      }
                                                  }
                                              }
                                          },
                                          (Command cmd, Function fn, string arg1, string arg2, string _ticket, LANG lang, Tid tid) {
                                              if (tid !is Tid.init)
                                              {
                                                  if (cmd == Command.Get && fn == Function.PropertyOfIndividual)
                                                  {
                                                      ResultCode rc;
                                                      string res1;
                                                      immutable(Individual)[ string ] onto_individuals =
                                                          context.get_onto_as_map_individuals();
                                                      immutable(Individual) individual = onto_individuals.get(arg1, _empty_iIndividual);
                                                      if (individual == _empty_iIndividual)
                                                      {
                                                          Ticket *ticket = context.get_ticket(_ticket);
                                                          if (ticket.result == ResultCode.OK)
                                                          {
                                                              immutable(Individual) individual1 =
                                                                  context.get_individual(ticket, arg1).idup;


                                                              immutable Resources res = individual1.resources.get(arg2, Resources.init);

                                                              foreach (rr; res)
                                                              {
                                                                  if (rr.lang == lang)
                                                                  {
                                                                      res1 = rr.data;
                                                                      break;
                                                                  }
                                                              }

                                                              rc = ResultCode.OK;
                                                          }
                                                          else
                                                              rc = ticket.result;
                                                      }
                                                      else
                                                      {
                                                          immutable Resources res = individual.resources.get(arg2, Resources.init);

                                                          foreach (rr; res)
                                                          {
                                                              if (rr.lang == lang)
                                                              {
                                                                  res1 = rr.data;
                                                                  break;
                                                              }
                                                          }
                                                          rc = ResultCode.OK;
                                                      }

                                                      send(tid, res1, rc);
                                                  }
                                              }
                                          },
                                          (Variant v) { writeln("pacahon_driver::Received some other type. ", v); }
                                          );
                              }
                          });
    }
}