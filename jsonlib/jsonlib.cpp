#include "rapidjson/document.h"
#include "rapidjson/writer.h"
#include "rapidjson/stringbuffer.h"
#include <iostream>
#include <sstream>
#include <typeinfo>

using namespace rapidjson;
extern "C"{

int* json(const char*s){
	std::cout << "Storing: " << s << std::endl;
	Document* d = new Document();
	(*d).Parse(s);
	return (int*)d;
}


Value& getp(int* intdoc, const char* key){
	Document* d = (Document*)intdoc;
	return (*d)[key];
}



const char* tostring(int *tempdoc){
	try{
		Value& typ = getp(tempdoc, "prim_type");
		if(typ.GetString() == "num"){
			std::stringstream strdoub;
			strdoub << getp(tempdoc, "prim_val").GetDouble();
			return strdoub.str().c_str();
		}
		else if(typ.GetString() == "str"){
			Value& pt = getp(tempdoc, "prim_val");
			return pt.GetString();
		}	
	}
	catch(...){
		Document* d = (Document *)tempdoc;
		StringBuffer buff;
		buff.Clear();
		Writer<StringBuffer> writer(buff);
		(*d).Accept(writer);
		return buff.GetString();
	}
}

int print(auto thing2print){
	std::cout<<(typeid(thing2print).name())<<std::endl;
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


//Retrieve a double in json
double get_json_double(int* intdoc){
	Value& typ = getp(intdoc, "prim_type");
	if(typ.GetString() == "num")
		return getp(intdoc, "prim_val").GetDouble();
	//Value& pt = getp(intdoc, "prim_val");
	//return pt.GetDouble();
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


//Retrieve a string in json
const char* get_json_string(int* intdoc){
	Value& pt = getp(intdoc, "json_rep_of_str_ts");
	return pt.GetString();
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

const char* jgets(int* intdoc, const char* key){
	Document* d = (Document*) intdoc;
	if((*d).HasMember(key)){
		std::cout << "we got it" << std::endl;
		return getp((int*)d, key).GetString();
	}
	else{
		std::cout << "not here" << std::endl;
		return "";
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
	if (elem == ((int *)(*((Document*)(intdoc))).End()))
		return NULL;
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
	std::cout << sizeof(int) <<std::endl;
	int dd = 5;
	print(dd);
	int* j = json(test);

	//testing adds
	adds(j, "boop", "is");
	adds(j, "test", "w");

	//testing string
	std::cout << tostring(j) << std::endl;
	std::cout << (*((Document*)j))["test"].GetString() << std::endl;

	int* d = json_double(3);
	int* d2 = json_double(5);
	
	//int* s = json_string("waduuuup");
	int* pts[] = {d, d2};
	int* pt = json_array(pts, 2);

	//std::cout << get_json_double(d) << std::endl;
	//std::cout << get_json_string(s) << std::endl;
	//std::cout << get_json_double(get_json_from_array(pt,0)) << std::endl;

	std::cout << double_to_string(d)<< std::endl;

	int* arr_itr = create_arr_iter(pt);

	print(pt);
	while(arr_itr){
		std::cout << get_json_double(arr_itr) <<std::endl;
		arr_itr = arr_next_elem(arr_itr, pt);
	}


	return 0;
}
*/
}
