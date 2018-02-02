#define _GLIBCXX_USE_CXX11_ABI    0

#include <string.h>
#include <iostream>
#include <math.h>

#include "msgpack8individual.h"

uint32_t write_individual(Individual *individual, char *in_buff)
{
    msgpack::sbuffer buffer;
    msgpack::packer<msgpack::sbuffer> pk(&buffer);

    pk.pack_array(2);
    pk.pack(individual->uri);
    pk.pack_map(individual->resources.size());
    map < string, vector <Resource> >::iterator p;
    for (p = individual->resources.begin(); p != individual->resources.end(); ++p)
    {
        std::string strKey = p->first;
        
        write_resources(p->first, p->second, pk);
    }

    memcpy(in_buff, buffer.data(), buffer.size());

    //for (int i  = 0; i < individual->resources["rdfs:label"].size(); i++)
    //    if (individual->resources["rdfs:label"][i].str_data.find("Пупкин Вася") != string::npos) {
    //        std::cerr << "MSGPACK: " << string(buffer.data(), buffer.size()) << endl;
    //        break;
    //    }
    return buffer.size();
}

void write_resources(string uri, vector <Resource> vv, msgpack::packer<msgpack::sbuffer> &pk)
{
    pk.pack(uri);
    pk.pack_array(vv.size());

    for (uint32_t i = 0; i < vv.size(); i++)
    {
        Resource value = vv[ i ];
        if (value.type == _Uri)
            pk.pack(value.str_data);
        else if (value.type == _Integer)
            pk.pack(value.long_data);
        else if (value.type == _Boolean)
            pk.pack(value.bool_data);
        else if (value.type == _Datetime)
        {
            pk.pack_array(2);
            pk.pack((uint)_Datetime);
            pk.pack(value.long_data);
        }
        else if (value.type == _Decimal)
        {
            pk.pack_array(3);
            pk.pack((uint)_Decimal);
            pk.pack(value.decimal_mantissa_data);
            pk.pack(value.decimal_exponent_data);
        }
        else
            if (value.lang != LANG_NONE)
            {
                pk.pack_array(3);
                pk.pack((uint)_String);
                pk.pack(value.str_data);
                pk.pack(value.lang);
            }
            else
            {
                pk.pack_array(2);
                pk.pack((uint)_String);
                pk.pack(value.str_data);
            }
    }
}
/////////////////////////////////////////////////////////////////////////////////////

int32_t msgpack2individual(Individual *individual, string in_str)
{
    msgpack::unpacker unpk;
    

    unpk.reserve_buffer(in_str.length());
    memcpy(unpk.buffer(), in_str.c_str(), in_str.length());
    unpk.buffer_consumed(in_str.length());
    msgpack::object_handle result;
    unpk.next(result);
    msgpack::object glob_obj(result.get()); 
    msgpack::object_array obj_arr = glob_obj.via.array;

    if (obj_arr.size != 2)
        return -1;
    
    msgpack::object *obj_uri = obj_arr.ptr;
    msgpack::object *obj_map = obj_arr.ptr + 1;
    
    individual->uri = string(obj_uri->via.str.ptr, obj_uri->via.str.size);
    
    // std::cerr << glob_obj << endl;
    // std::cerr << "URI " << uri << endl;

    msgpack::object_map map = obj_map->via.map;
    // std::cerr << "MAP_SIZE " << map.size << endl;
    
    for (int i = 0; i < map.size; i++ ) {
        // std::cerr << "\tKEY "  << *obj << endl;
        // std::cerr << "\tKEY: " << pair->key << " VALUE: " << pair->val << endl;
        msgpack::object_kv pair = map.ptr[i];
        msgpack::object key = pair.key;
        msgpack::object_array res_objs = pair.val.via.array;
        if (key.type != msgpack::type::STR) 
        {
            std::cerr << "@ERR! PREDICATE IS NOT STRING!" << endl;
            return -1;
        }

        std::string predicate(key.via.str.ptr, key.via.str.size);
        vector <Resource> resources;
        

        // std::cerr << "SIZE " << res_objs.size << endl;
        for (int j = 0; j < res_objs.size; j++)
        {
            msgpack::object value = res_objs.ptr[j];
            
            switch (value.type) 
            {
                case msgpack::type::ARRAY:
                {
                // std::cerr << "is array" << endl;
                    // std::cerr << "\t\t\tTRY ARR SIZE ";
                    msgpack::object_array res_arr = value.via.array;
                    // std::cerr << "ARR SIZE " << res_arr.size << endl; 
                    if (res_arr.size == 2)
                    {
                        long type = res_arr.ptr[0].via.u64;

                        if (type == _Datetime)
                        {
                            long value; 

                            Resource rr;
                            rr.type      = _Datetime;
                            if (res_arr.ptr[1].type == msgpack::type::POSITIVE_INTEGER)
                                rr.long_data = res_arr.ptr[1].via.u64;
                            else
                                rr.long_data = res_arr.ptr[1].via.i64;
                                
                            resources.push_back(rr);
                        }
                        else if (type == _String)
                        {
                            Resource    rr;

                            // std::cerr << "string" << endl;
                            rr.type = _String;
                            
                            if (res_arr.ptr[1].type == msgpack::type::STR)
                                rr.str_data = string(res_arr.ptr[1].via.str.ptr, 
                                    res_arr.ptr[1].via.str.size);
                            else if (res_arr.ptr[1].type == msgpack::type::NIL)
                                rr.str_data = "";
                            else 
                            {
                                std::cerr << "@ERR! NOT A STRING IN RESOURCE ARRAY 2" << endl;
                                return -1;
                            }

                            rr.lang = LANG_NONE;
                            resources.push_back(rr);
                        }
                        else
                        {
                            std::cerr << "@1" << endl;
                            return -1;
                        }
                    }
                    else if (res_arr.size == 3)
                    {
                        long type = res_arr.ptr[0].via.u64;
                        // std::cerr << "TYPE " << type << endl;
                        if (type == _Decimal)
                        {
                            long mantissa, exponent;
                            // std::cerr << "is decimal" << endl << "\t\t\t\tTRY MANTISSA";
                            if (res_arr.ptr[1].type == msgpack::type::POSITIVE_INTEGER)
                                mantissa = res_arr.ptr[1].via.u64;
                            else
                                mantissa = res_arr.ptr[1].via.i64;
                            // std::cerr << mantissa << endl << "\t\t\t\tTRY EXP";
                            if (res_arr.ptr[2].type == msgpack::type::POSITIVE_INTEGER)
                                exponent = res_arr.ptr[2].via.u64;
                            else
                                exponent = res_arr.ptr[2].via.i64;

                            
                            // std::cerr << exponent << endl;

                            Resource rr;
                            rr.type                  = _Decimal;
                            rr.decimal_mantissa_data = mantissa;
                            rr.decimal_exponent_data = exponent;
                            resources.push_back(rr);
                        }
                        else if (type == _String)
                        {
                            Resource    rr;
                            
                            rr.type = _String;
                            if (res_arr.ptr[1].type == msgpack::type::STR)
                                rr.str_data = string(res_arr.ptr[1].via.str.ptr, 
                                    res_arr.ptr[1].via.str.size);
                            else if (res_arr.ptr[1].type == msgpack::type::NIL)
                                rr.str_data = "";
                            else 
                            {
                                std::cerr << "@ERR! NOT A STRING IN RESOURCE ARRAY 2" << endl;
                                return -1;
                            }
                
                            long lang = res_arr.ptr[2].via.u64;
                            rr.lang     = lang;
                            resources.push_back(rr);

                        }
                        else
                        {
                            std::cerr << "@2" << endl;
                            return -1;
                        }
                    }
                    else
                    {
                        std::cerr << "@3" << endl;
                        return -1;
                    }
                    break;
                }

                case msgpack::type::STR:
                {
                    Resource    rr;
                    rr.type = _Uri;
                    rr.str_data = string(string(value.via.str.ptr, value.via.str.size));
                    resources.push_back(rr);
                    break;
                }

                case msgpack::type::POSITIVE_INTEGER:
                {
                    Resource rr;
                    rr.type      = _Integer;
                    rr.long_data = value.via.u64;
                    resources.push_back(rr);
                    break;
                }

                case msgpack::type::NEGATIVE_INTEGER:
                {
                    Resource rr;
                    rr.type      = _Integer;
                    rr.long_data = value.via.i64;
                    resources.push_back(rr);
                    break;
                }       

                case msgpack::type::BOOLEAN:
                {
                    Resource rr;
                    rr.type      = _Boolean;
                    rr.bool_data = value.via.boolean;
                    resources.push_back(rr);
                    break;
                } 

                default:
                {
                    std::cerr << "@ERR! UNSUPPORTED RESOURCE TYPE " << value.type << endl;
                    return -1;  
                }  
            }
        }

        // std::cerr << "RES SIZE " << resources.size() << endl;
        individual->resources[ predicate ] = resources;        
    }

    // std::cerr << individual << endl;
    // std::cerr << "END" << endl;
    //for (int i  = 0; i < individual->resources["rdfs:label"].size(); i++)
    //    if (individual->resources["rdfs:label"][i].str_data.find("Пупкин Вася") != string::npos) {
    //        std::cerr << "INDIVIDUAL BEGIN" << endl;
    //        individual->print_to_stderr();
    //        std::cerr << "INDIVIDUAL END" << endl;
    //        break;
    //    }
    return (int32_t)in_str.size();
}

int32_t individual2msgpack(Individual *individual, char *in_buff)
{
    return write_individual(individual, in_buff);
}

