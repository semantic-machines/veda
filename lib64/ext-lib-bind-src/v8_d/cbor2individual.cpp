#include "cbor.h"

#include <string.h>
#include <iostream>
#include <math.h>
#include "cbor2individual.h"


Element read_element(Individual *individual, const char *src, int b_pos, int e_pos, string subject_uri, string predicate_uri)
{
//	hexdump(src + b_pos, 100);
//	std::cout << "@c #read_element1 b_pos=" << b_pos << ", e_pos=" << e_pos << ", subject=" << subject_uri << ", predicate=" << predicate_uri << std::endl;
    ElementHeader header;
    Element       element;

    int           size = e_pos;

    element.pos = read_type_value(src, 0 + b_pos, size, &header);
    //writeln ("read_element:[", (uint)src[0], " ", (uint)src[1], "]");
    //writeln ("#^read_element, header=", header);
    //writeln ("read_element:[", (string)src[0..pos+header.len], "],[", src[0..pos+header.len], "]");

//	std::cout << "@c #read_element2 header.type=" << header.type  << std::endl;

    if (header.type == MAP)
    {
        //writeln("IS MAP, length=", header.len, ", pos=", pos);
        string  new_subject_uri;
        Element el_key = read_element(individual, src, b_pos + element.pos, size, "", "");
        element.pos += el_key.pos;

        Element el_val = read_element(individual, src, b_pos + element.pos, size, "", "");
        element.pos += el_val.pos;

        if (el_key.str == "@")
        {
            if (subject_uri.empty() == false)
            {
                // здесь новый индивидуал
            }
            else
            {
            }

//            Handle<Value> key = String::NewFromUtf8(isolate, el_key.str.c_str());
//            Handle<Value> value = String::NewFromUtf8(isolate, el_val.str.c_str());
            individual->uri = el_val.str;

            new_subject_uri = el_val.str;

            //writeln ("@ id:", val);
        }

        for (int i = 1; i < header.v_long; i++)
        {
            Element el_key_i = read_element(individual, src, b_pos + element.pos, size, "", "");
            element.pos += el_key_i.pos;

            string  new_predicate_uri = el_key_i.str;
            Element el_val_i          =
                read_element(individual, src, b_pos + element.pos, size, new_subject_uri, new_predicate_uri);
            element.pos += el_val_i.pos;
        }
    }
    else if (header.type == TEXT_STRING)
    {
        //writeln ("IS STRING, length=", header.len, ", pos=", pos);
        uint16_t    ep = (uint16_t)(element.pos + header.v_long);

        std::string str(src + b_pos + element.pos, header.v_long);

        element.str = str;

        //std::cout << "@c #read_element3 str=" << str << std::endl;
        //writeln ("[", str, "]");

        if (subject_uri.empty() == false && predicate_uri.empty() == false)
        {
            //writeln ("*1");
            vector <Resource> resources = individual->resources[ predicate_uri ];

            Resource          rr;

            if (header.tag == TEXT_RU)
            {
                rr.type     = _String;
                rr.str_data = str;
                rr.lang     = LANG_RU;
                resources.push_back(rr);
            }
            else if (header.tag == TEXT_EN)
            {
                rr.type     = _String;
                rr.str_data = str;
                rr.lang     = LANG_EN;
                resources.push_back(rr);
            }
            else if (header.tag == URI)
            {
                if (str.find("/") > 0)
                {
                    rr.type     = _Uri;
                    rr.str_data = str;
                    rr.origin   = _external;
                    resources.push_back(rr);
                }
                else
                {
                    rr.type     = _Uri;
                    rr.str_data = str;
                    rr.origin   = _local;
                    resources.push_back(rr);
                }
            }
            else
            {
                rr.type     = _String;
                rr.str_data = str;
                rr.lang     = LANG_NONE;
                resources.push_back(rr);
            }

            individual->resources[ predicate_uri ] = resources;
        }

        element.pos = ep;
    }
    else if (header.type == NEGATIVE_INTEGER)
    {
        vector <Resource> resources = individual->resources[ predicate_uri ];
        int64_t           value     = header.v_long;

        if (header.tag == EPOCH_DATE_TIME)
        {
            Resource rr;
            rr.type      = _Datetime;
            rr.long_data = value;
            resources.push_back(rr);
            individual->resources[ predicate_uri ] = resources;
        }
        else
        {
            Resource rr;
            rr.type      = _Integer;
            rr.long_data = value;
            resources.push_back(rr);
            individual->resources[ predicate_uri ] = resources;
        }
    }
    else if (header.type == UNSIGNED_INTEGER)
    {
        vector <Resource> resources = individual->resources[ predicate_uri ];

        int64_t           value = header.v_long;

        if (header.tag == EPOCH_DATE_TIME)
        {
            Resource rr;
            rr.type      = _Datetime;
            rr.long_data = value;
            resources.push_back(rr);
            individual->resources[ predicate_uri ] = resources;
        }
        else
        {
            Resource rr;
            rr.type      = _Integer;
            rr.long_data = value;
            resources.push_back(rr);
            individual->resources[ predicate_uri ] = resources;
        }
    }
    else if (header.type == FLOAT_SIMPLE)
    {
        vector <Resource> resources = individual->resources[ predicate_uri ];

        if (header.v_long == _TRUE)
        {
            Resource rr;
            rr.type      = _Boolean;
            rr.bool_data = true;
            resources.push_back(rr);
            individual->resources[ predicate_uri ] = resources;
        }
        else if (header.v_long == _FALSE)
        {
            Resource rr;
            rr.type      = _Boolean;
            rr.bool_data = false;
            resources.push_back(rr);
            individual->resources[ predicate_uri ] = resources;
        }
        else
        {
        }
    }
    else if (header.type == ARRAY)
    {
        if (header.tag == DECIMAL_FRACTION)
        {
            vector <Resource> resources = individual->resources[ predicate_uri ];

            ElementHeader     exponent;
            element.pos += read_type_value(src, b_pos + element.pos, size, &exponent);

            ElementHeader mantissa;
            element.pos += read_type_value(src, b_pos + element.pos, size, &mantissa);

            Resource rr;
            rr.type                  = _Decimal;
            rr.decimal_mantissa_data = mantissa.v_long;
            rr.decimal_expanent_data = exponent.v_long;
            resources.push_back(rr);
            individual->resources[ predicate_uri ] = resources;
        }
        else
        {
            //writeln ("IS ARRAY, length=", header.len, ", pos=", pos);
            for (int i = 0; i < header.v_long; i++)
            {
                Element el_val_i = read_element(individual, src, b_pos + element.pos, size, subject_uri, predicate_uri);
                element.pos += el_val_i.pos;
            }
        }
    }
    else if (header.type == TAG)
    {
//      std::cout << "@c #read_element IS TAG" << std::endl;
    }
    return element;
}

void write_individual(Individual *individual, std::vector<char> &ou)
{
    uint64_t  map_len = individual->resources.size() + 1;
    MajorType type    = MAP;

    write_type_value(type, map_len, ou);
    write_string("@", ou);
    write_string(individual->uri, ou);

    map < string, vector <Resource> >::iterator p;

    for (p = individual->resources.begin(); p != individual->resources.end(); ++p)
    {
        std::string strKey = p->first;
        write_resources(p->first, p->second, ou);
    }
}

void write_resources(string uri, vector <Resource> vv, std::vector<char> &ou)
{
    //std::cout << "@c #write_resources1 uri=" << uri << ", vv.size=" << vv.size() << std::endl;

    write_string(uri, ou);
    if (vv.size() > 1)
        write_type_value(ARRAY, vv.size(), ou);

    for (int i = 0; i < vv.size(); i++)
    {
        Resource value = vv[ i ];
        if (value.type == _Uri)
        {
	    //if (value.str_data.length () > 0)
	    {
        	write_type_value(TAG, URI, ou);
        	write_string(value.str_data, ou);
        	  //  std::cout << "@c #write uri data=" << value.str_data << std::endl;
	    }
        }
        else if (value.type == _Integer)
        {
            write_integer(value.long_data, ou);
        }
        else if (value.type == _Datetime)
        {
            write_type_value(TAG, EPOCH_DATE_TIME, ou);
            write_integer(value.long_data, ou);
        }
        else if (value.type == _Decimal)
        {
            write_type_value(TAG, DECIMAL_FRACTION, ou);
            write_type_value(ARRAY, 2, ou);
            write_integer(value.decimal_mantissa_data, ou);
            write_integer(value.decimal_expanent_data, ou);
        }
        else if (value.type == _Boolean)
        {
            write_bool(value.bool_data, ou);

            //std::cout << "@c #write_resources2 data=" << value.bool_data << std::endl;
        }
        else
        {
	    //if (value.str_data.length () > 0)
	    {
//std::cout << "@c#1 " << " value.lang= " <<  (uint64_t)value.lang << std::endl;

        	if (value.lang != LANG_NONE)
            	    write_type_value(TAG, value.lang + 41, ou);
        	write_string(value.str_data, ou);	

        	   // std::cout << "@c #write string data=" << value.str_data << std::endl;
	    }
        }
    }
}

/////////////////////////////////////////////////////////////////////////////////////


void cbor2individual(Individual *individual, string in_str)
{
//    hexdump(abStack, 100);
	//std::cout << "@c cbor2individual #1" << std::endl;
//    char *data_ptr = reinterpret_cast<char *>(&in_str[ 0 ]);
    const char *data_ptr = in_str.c_str();
	//std::cout << "@c cbor2individual #2" << std::endl;

    read_element(individual, data_ptr, 0, in_str.size(), "", "");
	//std::cout << "@c cbor2individual #3" << std::endl;
}

void individual2cbor(Individual *individual, std::vector<char> &ou)
{
    write_individual(individual, ou);
}

