#include <QtTest>
#include <iostream>
#include "../libtwin/twin.hpp"

// add necessary includes here

class twinTest : public QObject
{
    Q_OBJECT

public:
    twinTest();
    ~twinTest();

private slots:
    void test_case1();

};

twinTest::twinTest()
{

}

twinTest::~twinTest()
{

}

void twinTest::test_case1()
{
    auto t = twin::makeTwin(0,100, 100, twin::easing::bounceIn);
    t.step(5);
    std::cout << t.get() << "\n";
}

QTEST_APPLESS_MAIN(twinTest)

#include "tst_twintest.moc"
