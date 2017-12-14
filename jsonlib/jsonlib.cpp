#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"
#include <iostream>
#include <sstream>
using namespace rapidjson;
extern "C"{

int* is_json_double(int*);
int* is_json_string(int*);


Value& getp(int* intdoc, const char* key){
	Document* d = (Document*)intdoc;
	return (*d)[key];
}


const char* tostring(int* tempdoc){
	std::cout.flush();
	if((*((Document*) tempdoc)).IsObject()){
		Value* str = (Value*)tempdoc;
		if(str->IsString())
			return str->GetString();
		if((*((Document*) tempdoc)).HasMember("prim_type")){
			Value& typ = getp(tempdoc, "prim_type");
			if(typ.GetString() == "num"){
				std::ostringstream strdoub;
				strdoub << getp(tempdoc, "prim_val").GetDouble() << '\0';
				return strdoub.str().c_str();
			}
			else if(typ.GetString() == "str"){
				Value& pt = getp(tempdoc, "prim_val");
				return pt.GetString();
			}
			else if(typ.GetString() == "bool"){	
				Value& pt = getp(tempdoc, "prim_val");
				if(pt.GetBool())
					return "true";
				else
					return "false";
			}
		}
		else {
			Document* d = (Document *)tempdoc;
			StringBuffer buff;
			buff.Clear();
			Writer<StringBuffer> writer(buff);
			(*d).Accept(writer);	
			std::ostringstream objstr;
			objstr << buff.GetString() << '\0';
			return objstr.str().c_str();
		}
	}
	else{
		Document* d = (Document *)tempdoc;
		for (Value::ConstValueIterator itr = (*d).Begin(); itr != (*d).End(); ++itr){
			tostring((int *) itr);
		}
		StringBuffer buff;
		buff.Clear();
		Writer<StringBuffer> writer(buff);
		(*d).Accept(writer);
		std::ostringstream objstr;
		objstr << buff.GetString() << '\0';
		return objstr.str().c_str();
	}
}

int* json_bool(int b){
	Document *d = new Document();
	(*d).SetObject();
	if(b==1){
		(*d).AddMember("prim_type", "bool", (*d).GetAllocator());
		(*d).AddMember("prim_val", true, (*d).GetAllocator());
	}
	else{
		(*d).AddMember("prim_type", "bool", (*d).GetAllocator());
		(*d).AddMember("prim_val", false, (*d).GetAllocator());
	}
	return (int*)d;
}

double get_json_bool(int* intdoc){
	Value& pt = getp(intdoc, "prim_type");
	if(pt.GetString() == "bool"){
		if(getp(intdoc, "prim_val").GetBool())
			return 1;
		return 0;
	}
	return 0;
}


int* json(int* s){
	const char* str = tostring(s); 
	Document* d = new Document();
	(*d).Parse(str);
	return (int*)d;
}


int* is_json_object(int* s){
	Document *d = (Document *) s;
	if((*d).IsObject() && !get_json_bool(is_json_double(s)) && !get_json_bool(is_json_string(s)))
		return json_bool(1);
	return json_bool(0);
}

//Create a double in json by creating a json object with json_rep_of_num_ts as key
int* json_double(double dubs){
	Document *d = new Document();
	(*d).SetObject();
	Value db(dubs);
	(*d).AddMember("prim_type", "num", (*d).GetAllocator());
	(*d).AddMember("prim_val", dubs, (*d).GetAllocator());
	return (int*)d;
}

int * is_json_double(int* intdoc){
	Document *d = (Document *) intdoc;
	if((*d).IsObject() && (*d).HasMember("prim_type")){
		Value& typ = getp(intdoc, "prim_type");
		if (typ.GetString() == "num")
			return json_bool(1);
		return json_bool(0);
	}
	return json_bool(0);
}

//Retrieve a double in json
double get_json_double(int* intdoc){
	Value& pt = getp(intdoc, "prim_type");
	if(pt.GetString() == "num")
		return getp(intdoc, "prim_val").GetDouble();
	else if(pt.GetString() == "bool");
		return get_json_bool(intdoc);
	return pt.GetDouble();
}


//Create a string in json by creating a json object wit json_rep_of_str_ts as key
int* json_string(const char* s){
	Document *d = new Document();
	(*d).SetObject();
	Value tempvalue;
	tempvalue.SetString(s, (*d).GetAllocator());
	(*d).AddMember("prim_type", "str", (*d).GetAllocator());
	(*d).AddMember("prim_val", tempvalue.Move(), (*d).GetAllocator());
	return (int*)d;

}

int* is_json_string(int* intdoc){
	Document *d = (Document *) intdoc;
	if((*d).IsObject() && (*d).HasMember("prim_type")){
		Value& typ = getp(intdoc, "prim_type");
		if (typ.GetString() == "str")
			return json_bool(1);
		return json_bool(0);
	}
	return json_bool(0);
}


//Create an array in json from json values/docs (by coyping each value/doc into a new value and adding that to our new doc
int* json_array(int* a[], int numElements){
	Document *d = new Document();
	(*d).SetArray();
	Document::AllocatorType& allocator = (*d).GetAllocator();
	for(int i = 0; i < numElements; i++){
		Value tempdoc;
	       	tempdoc.CopyFrom(*((Document *)(a[i])), allocator);
		(*d).PushBack(tempdoc, allocator);
	}
	return (int *)d;
}

int* is_json_array(int* intdoc){
	Document *d = (Document *) intdoc;
	if((*d).IsArray())
		return json_bool(1);
	return json_bool(0);
}

//Return a pointer to the value at the idxth position
int* get_json_from_array(int* arr, int idx){
	Document* d = (Document *) arr;
	return (int *)(&((*d)[idx]));
}


//Create a null in json by creating a json object with json_rep_of_null_ts as key
int* json_null(){
	Document *d = new Document();
	(*d).SetObject();
	(*d).AddMember("json_rep_of_null_ts", NULL, (*d).GetAllocator());
	return (int*)d;
}
//Retrieve a null in json
int* get_json_null(int* intdoc){
	Value& pt = getp(intdoc, "json_rep_of_null_ts");
	return (int*)&pt;
}

int test(const char* s){
	std::cout << "HI" << std::endl;
	return 3;
}

int* jgets(int* intdoc, int* key){
	Document* d = (Document*) intdoc;
	const char* skey = tostring(key);
	if((*d).HasMember(skey)){
		return (int*)(&(getp((int*)d, skey)));
	}
	else{
		return 0;
	}
}

std::string adds(int *intdoc, const char* key, const char* value){
	Document* d = (Document*)intdoc;
	if ((*d).HasMember(key)){
		(*d)[key].SetString(value, strlen(value), (*d).GetAllocator());
		return key;
	}
	else{
		Value tempkey;
		Value tempvalue;
		tempkey.SetString(key, (*d).GetAllocator());
		tempvalue.SetString(value, (*d).GetAllocator());
		(*d).AddMember(tempkey.Move(), tempvalue.Move(), (*d).GetAllocator());
		return key;
	}
}


int* create_arr_iter(int* jsonthingie){
	Document* d = (Document*)jsonthingie;
	Value::ConstValueIterator itr = (*d).Begin();
	return (int *) itr;
}

int* arr_next_elem(int* itr, int* intdoc){
	Value::ConstValueIterator iter = (Value::ConstValueIterator) itr;
	int* elem = (int*)(++iter);
	if (elem == ((int *)(*((Document*)(intdoc))).End())){
		return 0;
	}
	else
		return elem;
}
int* create_obj_iter(int* jsonthingie){
	Document *d = (Document*)jsonthingie;
	Value::ConstMemberIterator itr = (*d).MemberBegin();
	return (int *) &(*itr);
}
/*
// Test function
int main(){
	//testing parse
	const char* test = "{\"test\":\"christophe\"}";
<<<<<<< HEAD
=======
	std::cout << sizeof(int) <<std::endl;
>>>>>>> 2819b078101c1be2c044ab36b88749629b683c08
	int* j = json(test);

	//testing adds
	adds(j, "boop", "is");
	adds(j, "test", "w");

	//testing string
	//std::cout << tostring(j) << std::endl;
	//std::cout << (*((Document*)j))["test"].GetString() << std::endl;

	int* d = json_double(3);
	int* s = json_string("waduuuup");
	int* pts[] = {d, s};
	int* pt = json_array(pts, 2);

<<<<<<< HEAD
	//std::cout << get_json_double(d) << std::endl;
	//std::cout << get_json_string(s) << std::endl;
	//std::cout << get_json_double(get_json_from_array(pt,0)) << std::endl;


	int* arr_itr = create_arr_iter(pt);

	print(pt);
	while(arr_itr){
		std::cout << get_json_double(arr_itr) <<std::endl;
		arr_itr = arr_next_elem(arr_itr, pt);
	}

=======
	std::cout << get_json_double(d) << std::endl;
	std::cout << get_json_string(s) << std::endl;
	std::cout << get_json_double(get_json_from_array(pt,0)) << std::endl;
>>>>>>> 2819b078101c1be2c044ab36b88749629b683c08

	return 0;
}*/
}
