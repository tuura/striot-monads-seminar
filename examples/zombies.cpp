#include <tuple>
#include <vector>
#include <algorithm>    // std::random_shuffle
#include <cstdlib>      // std::rand, std::srand
#include <iostream>

using namespace std;

// random generator function:
int randomGen (int i) { return rand()%i;}

template <typename A>
vector<tuple<A, A>> pairs (const vector<A> & xs) {
    auto result = vector<tuple<A, A>>();
    for (auto i = 0; i < xs.size(); ++i) {
        for (auto j = i + 1; j < xs.size(); ++j) {
            result.push_back(make_tuple(xs[i], xs[j]));
        }
    }
    return result;
}

using Survivor = int;

using Group = vector<Survivor>;

using Shift = tuple<int, int>;

using Schedule = tuple<Shift, Shift, Shift>;

vector<Shift> shifts (const Group & people) {
    auto ps = pairs(people);
    random_shuffle(ps.begin(), ps.end(), randomGen);
    return ps;
}

Schedule buildSchedule (const Group & people) {
    auto shs = shifts(people);
    return make_tuple(shs[0], shs[1], shs[2]);
}

int main () {
    srand ( unsigned ( time(0) ) );
    const Group people{1,2,3,4,5};

    auto schedule = buildSchedule(people);

    cout << "Watch schedule for this night: ";
    cout << '(' << get<0>(get<0>(schedule)) << ',' << get<1>(get<0>(schedule)) << "), ";
    cout << '(' << get<0>(get<1>(schedule)) << ',' << get<1>(get<1>(schedule)) << "), ";
    cout << '(' << get<0>(get<2>(schedule)) << ',' << get<1>(get<2>(schedule)) << ')';
    cout << endl;

    return 0;
}
