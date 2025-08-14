// CW: AI generated test
// test_anon_struct.c

// 1. Named sub-struct, no tag
struct NamedNoTag {
    struct { int x; int y; } pos;  // named field 'pos', no struct tag
};

// 2. Named sub-struct, with tag
struct Inner { int x; int y; };
struct NamedWithTag {
    struct Inner pos;  // named field, tagged type
};

// 3. Anonymous sub-struct, no tag, no declarator
struct AnonymousMember {
    int z;
    struct { double x; int y; }; // anonymous member
};

static struct AnonymousMember d = { 8, 9.1, 200};


int main(void) {
    // Test 1
    struct NamedNoTag a = { { 1, 2 } };
    if (a.pos.x != 1 || a.pos.y != 2) return 1;

    // Test 2
    struct NamedWithTag b = { { 3, 4 } };
    if (b.pos.x != 3 || b.pos.y != 4) return 2;

    // Test 3
    struct AnonymousMember c = { 7, 5.1, 6 }; // z=7, x=5, y=6
    if (c.z != 7)
        return 3;
    if (c.x != 5.1)
        return 4;
    if (c.y != 6)
        return 5;

    // Test 3
    if (d.z != 8)
        return 6;
    if (d.x != 9.1)
        return 7;
    if (d.y != 200)
        return 8;

    return 0; // success
}
