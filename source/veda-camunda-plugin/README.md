1. Build plugin

$ mvn clean install

2. Install plugin to Tomcat server running camunda engine

Copy built `veda-camunda-plugin-VSN.jar` to Tomcat `$TOMCAT/lib/` directory

3. Configure camunda default engine to use plugin

Edit Tomcat `$TOMCAT/conf/bpm-platform.xml` file

Add the following plugin registration:

```
<process-engine name="default">
    ...
    <plugins>
    ...
      <plugin>
        <class>com.semanticmachines.veda.bpm.VedaParseListenerPlugin</class>
      </plugin>
    ...
    </plugins>
</process-engine name="default">
```