#include <json.hpp>

using namespace nlohmann;

int main()
{
    // create an array value
    json array = {1, 2, 3, 4, 5};

    // get am iterator to the first element
    json::iterator it = array.begin();

    // serialize the element that the iterator points to
    std::cout << *it << '\n';
}
