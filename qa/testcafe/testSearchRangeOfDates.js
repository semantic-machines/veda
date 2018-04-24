import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Search Range Of Dates`
    //.page `http://localhost:8080/`
    .page `http://live.semantic-machines.com`
  const basic = new Basic();
  test('testSearchRangeOfDates', async t => {
    basic.login('karpovrt', '123');
    basic.createPerson('Range', 'Of', 'DatesO', '01.01.2014');
    basic.createPerson('Range', 'Of', 'DatesO', '02.12.2015');
    basic.createPerson('Range', 'Of', 'DatesO', '12.07.2016');
    basic.createPerson('Range', 'Of', 'DatesO', '12.07.2017');
    basic.fullTextSearch('\'v-s:birthday\' == [2014-01-01T00:00:00, 2014-01-01T23:59:59] && \'v-s:middleName\' == \'DatesO\'', '1');
    basic.fullTextSearch('\'v-s:birthday\' == [2014-01-01T00:00:00, 2015-12-02T23:59:59] && \'v-s:middleName\' == \'DatesO\'', '2');
    basic.fullTextSearch('\'v-s:birthday\' == [2014-01-01T00:00:00, 2016-07-12T23:59:59] && \'v-s:middleName\' == \'DatesO\'', '3');
    basic.fullTextSearch('\'v-s:birthday\' == [2014-01-01T00:00:00, 2034-07-12T23:59:59] && \'v-s:middleName\' == \'DatesO\'', '4');
    await t
      .expect(Selector('ul.nav.navbar-nav.navbar-right li#user-info').innerText).eql('Администратор2\n');
});


