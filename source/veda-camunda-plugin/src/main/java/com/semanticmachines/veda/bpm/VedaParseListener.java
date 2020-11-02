package com.semanticmachines.veda.bpm;

import org.camunda.bpm.engine.impl.bpmn.behavior.UserTaskActivityBehavior;
import org.camunda.bpm.engine.impl.bpmn.parser.AbstractBpmnParseListener;
import org.camunda.bpm.engine.impl.pvm.delegate.ActivityBehavior;
import org.camunda.bpm.engine.impl.pvm.process.ActivityImpl;
import org.camunda.bpm.engine.impl.pvm.process.ScopeImpl;
import org.camunda.bpm.engine.impl.task.TaskDefinition;
import org.camunda.bpm.engine.impl.util.xml.Element;
import com.semanticmachines.veda.bpm.VedaTaskListener;

/**
 * BPMN Parse Listener to add task listener on user task
 *
 */
public class VedaParseListener extends AbstractBpmnParseListener {

  @Override
  public void parseUserTask(Element userTaskElement, ScopeImpl scope, ActivityImpl activity) {
    ActivityBehavior activityBehavior = activity.getActivityBehavior();
    if(activityBehavior instanceof UserTaskActivityBehavior ){
      UserTaskActivityBehavior userTaskActivityBehavior = (UserTaskActivityBehavior) activityBehavior;
      TaskDefinition task = userTaskActivityBehavior.getTaskDefinition();
      task.addTaskListener("create", VedaTaskListener.getInstance());
      task.addTaskListener("assignment", VedaTaskListener.getInstance());
      task.addTaskListener("update", VedaTaskListener.getInstance());
      task.addTaskListener("delete", VedaTaskListener.getInstance());
      task.addTaskListener("timeout", VedaTaskListener.getInstance());
      task.addTaskListener("complete", VedaTaskListener.getInstance());
    }
  }
}
