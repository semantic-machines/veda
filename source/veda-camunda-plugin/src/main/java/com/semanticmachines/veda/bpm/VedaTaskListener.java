package com.semanticmachines.veda.bpm;

import org.camunda.bpm.engine.delegate.DelegateTask;
import org.camunda.bpm.engine.delegate.TaskListener;

import java.util.ArrayList;
import java.util.List;
import java.util.logging.Logger;

/**
 * Task listener to be executed when a user task is entered
 */
public class VedaTaskListener implements TaskListener {

  // Possible events = create, assignment, complete, update, delete, timeout
	
  private final Logger LOGGER = Logger.getLogger(this.getClass().getName());
  public static long callCounter = 0;

  private static VedaTaskListener instance = null;

  protected VedaTaskListener() { }

  public static VedaTaskListener getInstance() {
    if(instance == null) {
      instance = new VedaTaskListener();
    }
    return instance;
  }

  public void notify(DelegateTask delegateTask) {
    callCounter++;
    String event = delegateTask.getEventName();
    LOGGER.info("["+ callCounter + "] Task event: '" + event + "', task: " + delegateTask);
    // Create individual in Veda with taskId and event type
  }

}
