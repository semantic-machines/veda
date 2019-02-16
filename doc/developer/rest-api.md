### Платформа Veda. Методы REST интерфейса:

|                      | **User authenticate**                                        |
| -------------------- | ------------------------------------------------------------ |
| **URL**              | /authenticate                                                |
| **Method**           | GET                                                          |
| **URL Params**       | **Required:** <br />`login=[string], password=[string]`<br /><br />**Optional:**<br />`secret=[string]` |
| **Success Response** | **Code:** 200 <br />**Sample content:** <br />`{ end_time : 636858783968914000, id : "a7e13ad5-f2d7-4f8f-8543-aceda5fc4718", result : 200, user_uri : "td:RomanKarpov" }` |
| **Error Response**   | **Code:** 472 or 500                                         |
| **Sample Call**      | `/authenticate?login=karpovrt&password=a665a45920422f9da04a1f3fff1fa07e998e86f7f7a27ae3` |



|                      | Check ticket validity                                        |
| -------------------- | ------------------------------------------------------------ |
| **URL**              | /is_ticket_valid                                             |
| **Method**           | GET                                                          |
| **URL Params**       | **Required:** <br />`ticket=[string]`                        |
| **Success Response** | **Code:** 200 <br />**Sample content:** true                 |
| **Error Response**   | **Code:** 472 or 500                                         |
| **Sample Call**      | /is_ticket_valid?ticket=f04f82a4-a7c2-4545-ba6c-b20f7022ca5c |



|                      | Get ticket for another user                                  |
| -------------------- | ------------------------------------------------------------ |
| **URL**              | /get_ticket_trusted                                          |
| **Method**           | GET                                                          |
| **URL Params**       | **Required:** <br />`ticket=[string], login=[string]`<br />  |
| **Success Response** | **Code:** 200 <br />**Sample content:**<br /> `{ end_time : 636858783968916000, id : "a7e13ad5-f846-4f8f-8543-aceda5fc4718", result : 200, user_uri : "td:MilliganBilly" }` |
| **Error Response**   | **Code:** 473 or 500<br /><br />**Content:**<br />`{ end_time : 0, id : "", result : 473, user_login : "", user_uri : ""}` |
| **Sample Call**      | /get_ticket_trusted?ticket=f04f82a4-a7c2-4545-ba6c-b20f7022ca5c&/authenticate?login=milliganb |



|                      | Execute full text query                                      |
| -------------------- | ------------------------------------------------------------ |
| **URL**              | /query                                                       |
| **Method**           | POST                                                         |
| **Request Params**   | **Required:**  `ticket:[string], query:[string]` <br /><br />**Optional:**<br />`sort:[string], databases:[string], reopen:[bool], from:[int], top:[int],                        limit:[int], trace:[bool]` |
| **Success Response** | **Code:** 200  <br />**Sample content:** <br />`{ result:["cfg:Administrator", "td:AleksandraKhvostikova", "td:RomanKarpov", "td:ValeriyBushenev", "td:AndreyBychin"], count:5, estimated:5, processed:5, cursor:5, result_code:200 }`<br />*or empty set if not found individuals*<br />`{result:[], count:0, estimated:0, processed:0, cursor:0, result_code:200}` |
| **Error Response**   | **Code:** 400 or 500 <br />**Content:** `{ result:[], count:0, estimated:0, processed:0, cursor:0, result_code :400 }` |
| **Sample Call**      | **Request:**<br />`{ ticket : "f04f82a4-a7c2-4545-ba6c-b20f2ca5c", query : "( 'rdf:type'=='v-s:UserThing' ) && ( '\*' == '+персона' )", sort : "'v-s:created' desc", reopen : false,  top : 10, limit : 100, from : 0 }` |



|                      | Get one individual                                           |
| -------------------- | ------------------------------------------------------------ |
| **URL**              | /get_individual                                              |
| **Method**           | GET                                                          |
| **URL Params**       | **Required:**  `ticket=[string], uri=[string]` <br /><br />**Optional:**   `reopen=[bool]` |
| **Success Response** | **Code:** 200  <br />**Sample content:**<br /> `{ "@": "v-ui:DefaultLanguage", "rdf:type" : [{data:"rdfs:Resource", type:"Uri"}], "rdf:value" : [{data:"v-ui:RU", type:"Uri"}], "rdfs:label" : [{ data :"Язык по-умолчанию","lang":"RU", type:"String"},{ data :"Default language","lang":"EN", type:"String"}], "v-s:updateCounter" : [{ data :1, type:"Integer"}]}` |
| **Error Response**   | **Code:** 473 or 500                                         |
| **Sample Call**      | /get_individual?ticket=f04f82a4-a7c2-4545-ba6c-b20f2ca5c&uri=v-ui:DefaultLanguage |



|                      | Get more individuals                                         |
| -------------------- | ------------------------------------------------------------ |
| **URL**              | /get_individuals                                             |
| **Method**           | POST                                                         |
| **Request Params**   | **Required:**  `ticket:[string], uris:[array of string]`  <br /><br />**Optional:**   `reopen:[bool]` |
| **Success Response** | **Code:** 200  <br />**Sample content:** <br />`[{"@":"v-s:WelcomeToVeda", "rdf:type":[{data:"v-s:Bundle", type:"Uri"}], "rdfs:label":[{data:"Добро пожаловать в Veda!", lang:"RU", type:"String"}, {data:"Welcome to Veda!", lang:"EN", type:"String"}], "v-s:updateCounter":[{data:1, type:"Integer"}]}, {"@":"v-s:VedaDescription", "rdf:type":[{data:"v-s:Bundle", type:"Uri"}], "rdfs:label":[{data:"Платформа для разработки web-приложений уровня предприятий.", lang:"RU", type:"String"}, {data:"Enterprise application development platform.", lang:"EN", type:"String"}], "v-s:updateCounter":[{data:1, type:"Integer"}]}]` |
| **Error Response**   | **Code:** 500                                                |
| **Sample Call**      | **Request:**<br />`{ ticket:"f04f82a4-a7c2-4545-ba6c-b20f7022ca5c", uris: ["v-s:WelcomeToVeda", "v-s:VedaDescription" ] }` |



|                      | Put one individual                                           |
| -------------------- | ------------------------------------------------------------ |
| **URL**              | /put_individual                                              |
| **Method**           | PUT                                                          |
| **Request Params**   | **Required:**  `ticket:[string], individual:[object]`  <br /><br />**Optional:**   `prepare_events:[bool], assigned_subsystems:[byte], event_id=[string], transaction_id:[string]` |
| **Success Response** | **Code:** 200   <br />**Sample content:** `{ op_id:12246, result:200 }` |
| **Error Response**   | **Code:** 473 or 500                                         |
| **Sample Call**      | **Request:**<br />`{ ticket:"f04f82a4-a7c2-4545-ba6c-b20f7022ca5c", individual:{"@":"v-s:Welcome", "rdf:type":[{data:"v-s:WelcomeAspect",type:"Uri"}], "rdfs:label":[{data:"Добро пожаловать", lang:"RU", type:"String"}, {data:"Welcome", lang:"EN",type:"String"}], "v-s:updateCounter":[{data:1,type:"Integer"}]}, assigned_subsystems:0, prepare_events:true, event_id:"", transaction_id:"" }` |

-------------------------------



GET get_rights
    Json get_rights(string ticket, string uri);

GET get_rights_origin
    Json[] get_rights_origin(string ticket, string uri);

GET get_membership
    Json get_membership(string ticket, string uri);

GET get_operation_state
    long get_operation_state(int module_id, long wait_op_id);

GET send_to_module
    void send_to_module(int module_id, string msg);

GET flush
    void flush(int module_id, long wait_op_id);

PUT put_individuals
    OpResult[] put_individuals(string ticket, Json[] individual, string event_id, long assigned_subsystems = 0);

PUT remove_individual
    OpResult remove_individual(string ticket, string uri, string event_id, long assigned_subsystems = 0);

PUT remove_from_individual
    OpResult remove_from_individual(string ticket, Json individual, string event_id, long assigned_subsystems = 0);

PUT set_in_individual
    OpResult set_in_individual(string ticket, Json individual, string event_id, long assigned_subsystems = 0);

PUT add_to_individual
    OpResult add_to_individual(string ticket, Json individual, string event_id, long assigned_subsystems = 0);