### Платформа Veda. Методы REST интерфейса:

|                      | **User Authenticate**                                        |
| -------------------- | ------------------------------------------------------------ |
| **URL**              | /authenticate                                                |
| **Method**           | GET                                                          |
| **URL Params**       | **Required:** <br />`login=[string], password=[string]`<br /><br />**Optional:**<br />secret=[string] |
| **Success Response** | **Code:** 200 <br />**Content:** `{"end_time":636858783968914000,"id":"a7e13ad5-f2d7-4f8f-8543-aceda5fc4718","result":200,"user_uri":"td:RomanKarpov"}` |
| **Error Response**   | **Code:** 472 or 500                                         |
| **Sample Call**      | `/authenticate?login=karpovrt&password=a665a45920422f9da04a1f3fff1fa07e998e86f7f7a27ae3` |



-------------------------------

GET get_rights
    Json get_rights(string ticket, string uri);

GET get_rights_origin
    Json[] get_rights_origin(string ticket, string uri);

GET get_membership
    Json get_membership(string ticket, string uri);

GET get_ticket_trusted
    Ticket get_ticket_trusted(string ticket, string login);

GET is_ticket_valid
    bool is_ticket_valid(string ticket);

GET get_operation_state
    long get_operation_state(int module_id, long wait_op_id);

GET send_to_module
    void send_to_module(int module_id, string msg);

GET flush
    void flush(int module_id, long wait_op_id);

GET query
    SearchResult query(string ticket, string query, string sort = null, string databases = null, bool reopen = false, int from = 0, int top = 10000,
                       int limit = 10000, bool trace = false);

POST get_individuals
    Json[] get_individuals(string ticket, string[] uris);

GET get_individual
    Json get_individual(string ticket, string uri, bool reopen = false);

PUT put_individual
    OpResult put_individual(string ticket, Json individual, string event_id, long assigned_subsystems = 0);

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