Платформа Veda.

Методы REST интерфейса:

-------------------------------

**Authorize resource**
----
  Returns calculate access byte

* **URL**

  /authorize

* **Method:**

  `GET`
  
*  **URL Params**

   **Required:**
 
   `ticket=[string], uri=[string], access=[unsigned byte]`

* **Success Response:**

  * **Code:** 200 <br />
    **Content:** `access=[unsigned byte]`
 
* **Error Response:**

  * **Code:** 404 NOT FOUND <br />

-------------------------------


GET get_rights
    Json get_rights(string ticket, string uri);

GET get_rights_origin
    Json[] get_rights_origin(string ticket, string uri);

GET get_membership
    Json get_membership(string ticket, string uri);

GET authenticate
    Ticket authenticate(string login, string password);

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

-----------------------------------

First Header       | Second Header
------------------ | -------------
Content cell 1     | Content cell 2
Content column 1   | Content column 2







