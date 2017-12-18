#ifndef JSONLIBH
#define JSONLIBH
#ifdef __cplusplus
extern "C" {
#endif
const char* tostring(int*);
int* json_bool(int);
int* is_json_bool(int*);
double get_json_bool(int*);
int* json_from_string(int*);
int* json_object(int*[], int);
int* get_json_from_object(int*, int*);
int* is_json_object(int*);
int* add_to_json_object(int*, int*, int*);
int* json_double(double);
int* to_json_double(int*);
int* is_json_double(int*);
double get_json_double(int*);
int* json_string(const char*);
int* is_json_string(int*);
int* json_array(int*[], int);
int* is_json_array(int*);
int* get_json_from_array(int*, int);
int* push_to_json_array(int*, int*);
int* replace_json_array_element(int*, int*, int*);
int* jgets(int*, int*);
int* create_arr_iter(int*);
int* arr_next_elem(int*, int*);
#ifdef __cplusplus
}
#endif
#endif
