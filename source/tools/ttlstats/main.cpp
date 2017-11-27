#include <iostream>
#include <string>
#include <sys/types.h>
#include <dirent.h>
#include <string.h>
#include <errno.h>
#include <raptor2/raptor2.h>
#include <map>
#include <vector>

using namespace std;

raptor_world *world;
raptor_parser *parser;
map<string, string> uris_in_file;
vector<pair<string, string> > links_in_file;

void prepare_triple(void *user_data, raptor_statement *triple)
{


    string fname = string((char *)user_data);
    // cout << fname << endl;

    if (triple->subject->type == RAPTOR_TERM_TYPE_URI || triple->subject->type == RAPTOR_TERM_TYPE_BLANK) {
        string uri = string((char *)raptor_term_to_string(triple->subject));
        string link = string((char *)raptor_term_to_string(triple->object));

        uris_in_file.insert(pair<string, string>(uri, fname));
        // cout << "check: " << uris_in_file[uri] << endl;
        links_in_file.push_back(pair<string, string>(link, fname));
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

    map<string, vector<string> > from_to;
    world = raptor_new_world();   
    parser = raptor_new_parser(world, "turtle");

    walk_directories(string(argv[1]));

    for (std::vector<pair<string, string> >::iterator it = links_in_file.begin(); it != links_in_file.end(); ++it)
    {
        string from = it->second;
        if (uris_in_file.find(it->first) != uris_in_file.end()) {
            string to = uris_in_file[it->first];
            if (from_to.find(from) == from_to.end()) 
                from_to.insert(pair<string, vector<string> >(from, vector<string>()));
            
            from_to[from].push_back(to);
            // cout << to << endl;
         /*   string fname = uris_in_file[it->first];
            cout << fname << endl;*/
        } else {
            // cout << "[" << it->first << "] from file " << it->second << " not found in other files" << endl;  
        }
    }

    for (map<string, vector<string> >::iterator it = from_to.begin(); it != from_to.end(); it++) {
        string fname = it->first;
        vector<string> to_fname = it->second;
        cout << fname << "[" << to_fname.size() << "] links" << endl;

        for (int i = 0; i < to_fname.size(); i++)
            cout << "\t" << to_fname[i] << endl;
    }
    return 0;
}