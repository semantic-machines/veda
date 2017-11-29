#include <iostream>
#include <string>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <errno.h>
#include <raptor2/raptor2.h>
#include <map>
#include <vector>
#include <fstream>
#include <sstream>

using namespace std;

raptor_world *world;
raptor_parser *parser;
map<string, pair<string, int> > uris_in_file;
vector<pair<string, pair<string, int> > > links_in_file;
map<string, string> prefixes;

void prepare_prefixes(void* user_data, raptor_namespace *nspace)
{
    raptor_uri *ruri = raptor_namespace_get_uri(nspace);
    if (ruri == NULL) {
        return;
    }

    char *curi = (char *)raptor_uri_to_string(ruri);
    if (curi == NULL) {
        return;
    }


    char *cprefix  = (char *)raptor_namespace_get_prefix(nspace);
    if (cprefix == NULL)
        return;

    string prefix = string(cprefix) + ":";
    string uri = string(curi);

    prefixes.insert(pair<string, string>(uri, prefix));
}

void prepare_triple(void *user_data, raptor_statement *triple)
{
    static char line[4096];

    string fname = string((char *)user_data);
    // cout << fname << endl;

    if (triple->subject->type == RAPTOR_TERM_TYPE_URI || triple->subject->type == RAPTOR_TERM_TYPE_BLANK) {
        string uri = string((char *)raptor_term_to_string(triple->subject));
        uri= uri.substr(1, uri.size()-2);
        // cout << "subject: " << uri << endl;
        if (prefixes.find(uri + "/") != prefixes.end()) {
       /*     cout << "found" << endl;
            cout << "\tsubject: " << uri << endl;*/
            uri = prefixes[uri +"/"];
            // cout << "\tprefix: " << uri << endl;
        } else if (prefixes.find(uri + "#") != prefixes.end()) {
        /*    cout << "found" << endl;
            cout << "\tsubject: " << uri << endl;*/
            uri = prefixes[uri +"#"];
            // cout << "\tprefix: " << uri << endl;
        } else {
            string separator = "/";
            if (uri.find("#") != string::npos) {
                // cout << "subject: " << uri << endl;
                separator = "#";
            }
            
            
            size_t pos = uri.find_last_of(separator);
            string long_prefix = uri.substr(0, pos+1);
            string short_prefix = prefixes[long_prefix];
            uri.replace(0, pos+1, short_prefix);
            // if (separator == "#") {
            // cout <<"uri: " << uri << endl; 
            // cout << "long_prefix: " << long_prefix << endl;
            // cout << "short_prefix: " << short_prefix << endl;
            // }

            

        }


        string link = string((char *)raptor_term_to_string(triple->object));
        link= link.substr(1, link.size()-2);
        // cout << "subject: " << link << endl;
        if (prefixes.find(link + "/") != prefixes.end()) {
       /*     cout << "found" << endl;
            cout << "\tsubject: " << link << endl;*/
            link = prefixes[link +"/"];
            // cout << "\tprefix: " << link << endl;
        } else if (prefixes.find(link + "#") != prefixes.end()) {
        /*    cout << "found" << endl;
            cout << "\tsubject: " << link << endl;*/
            link = prefixes[link +"#"];
            // cout << "\tprefix: " << link << endl;
        } else {
            string separator = "/";
            if (link.find("#") != string::npos) {
                // cout << "subject: " << link << endl;
                separator = "#";
            }
            
            
            size_t pos = link.find_last_of(separator);
            string long_prefix = link.substr(0, pos+1);
            string short_prefix = prefixes[long_prefix];
            link.replace(0, pos+1, short_prefix);
            // if (separator == "#") {
            // cout <<"link: " << link << endl; 
            // cout << "long_prefix: " << long_prefix << endl;
            // cout << "short_prefix: " << short_prefix << endl;
            // }
        }

        ifstream fin;
        fin.open(fname.c_str(), std::ifstream::in);
        int count = 0;
        int uri_line = -1, link_line = -1;
        while(fin.getline(line, 4096)) {
            count++;
            string line_str = string(line);
            
            if (line_str.find(uri) == 0) {
                uri_line = count;
                while (fin.getline(line, 4096)) {
                    count++;
                    line_str = string(line);
                    istringstream sin(line_str);
                    string file_link;
                    bool found = false;
                    while (sin >> file_link) { 
                        // cout << "link [" << link << "]" <<endl;
                        // cout << "file link [" << file_link << "]" <<endl;
                        if (link == file_link) {
                            link_line = count;
                            found = true;
                            break;
                        }
                    }

                    if (found)
                        break;
                }
             /*   cout << "file: " << fname << endl;
                cout << "\turi: " << uri << endl;
                cout << "\tlink " << link << endl;
                cout <<"\tline: " << count << endl;*/
                break;
            }
        }
        fin.close();

        uris_in_file.insert(pair<string, pair<string, int> >(uri, pair<string, int>(fname, uri_line)));
        // cout << "check: " << uris_in_file[uri] << endl;
        links_in_file.push_back(pair<string, pair<string, int> >(link, pair<string, int>(fname, link_line)));
    }
}

void walk_directories(string dir_name)
{
    struct dirent *ent;
    DIR *dir = opendir(dir_name.c_str());
    if (dir == NULL) {
        cout << dir_name << endl;
        cout << "Err reading directory: " << string(strerror(errno)) << endl;
        return;
    }
    
    while ((ent = readdir (dir)) != NULL) {
        string ent_name = string(ent->d_name);
        if (ent_name == ".." || ent_name == ".")
            continue;
        
        ent_name = dir_name  + "/" + ent_name;
        if (ent->d_type == DT_DIR) {
            walk_directories(ent_name);
            continue;
        }

    /*    std::size_t found = str.find(str2);
  if (found!=std::string::npos)
    std::cout << "first 'needle' found at: " << found << '\n';*/

        if (ent_name.find(".ttl") == string::npos)
            continue;

        unsigned char *file_uri_str = raptor_uri_filename_to_uri_string(ent_name.c_str());
        raptor_uri *file_uri = raptor_new_uri(world, file_uri_str);
        raptor_uri *file_uri_copy = raptor_uri_copy(file_uri);
        raptor_parser_set_namespace_handler(parser, (void *)ent_name.c_str(), prepare_prefixes);
        raptor_parser_set_statement_handler(parser, (void *)ent_name.c_str(), prepare_triple);
        raptor_parser_parse_file(parser, file_uri, file_uri_copy);
    }
}

int main(int argc, char** argv)
{
    if (argc < 2) {
        cerr << "Need path to ontology root as argument" << endl;
        return 0;
    }

    map<string, vector<tuple<int, string, int> > > from_to;
    vector<tuple<string, int, string> > links_to_nowhere;
    world = raptor_new_world();   
    parser = raptor_new_parser(world, "turtle");

    walk_directories(string(argv[1]));

    for (vector<pair<string, pair<string, int> > >::iterator it = links_in_file.begin(); it != links_in_file.end(); ++it)
    {
        string from = it->second.first;
        int from_line = it->second.second;
        if (uris_in_file.find(it->first) != uris_in_file.end()) {
            string to = uris_in_file[it->first].first;
            int to_line = uris_in_file[it->first].second;
            if (from_to.find(from) == from_to.end()) 
                from_to.insert(pair<string, vector<tuple<int, string, int> > >(from, 
                    vector<tuple<int, string, int> >()));
            
            from_to[from].push_back(tuple<int, string, int> (from_line, to, to_line));

        } else {
            cout << "link not found: " << it->first << endl;
            links_to_nowhere.push_back(tuple<string,int, string>(from, from_line, it->first));
        }
    }

    for (map<string, vector<tuple<int, string, int> > >::iterator it = from_to.begin(); 
        it != from_to.end(); it++) {
        string fname = it->first;
        vector<tuple<int, string, int> > tuples = it->second;
        for (int i = 0; i < tuples.size(); i++) {
            int from_line = get<0>(tuples[i]);
            string to_fname = get<1>(tuples[i]);
            int to_line = get<2>(tuples[i]);
            if (fname != to_fname)
                cout << fname << "(" << from_line << ") " << "->" << to_fname << "(" << 
                    to_line << ")" << endl;
        }
    }

    cout << endl << "links to nowhere: " << endl;
    for (int i = 0; i < links_to_nowhere.size(); i++) {
        string from = get<0>(links_to_nowhere[i]);
        int from_line = get<1>(links_to_nowhere[i]);
        string link = get<2>(links_to_nowhere[i]);
         cout << from << "(" << from_line << ") " << link << endl;
    }
    return 0;
}