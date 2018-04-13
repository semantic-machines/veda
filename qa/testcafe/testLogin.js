import Basic from './basic'
import { Selector, t } from 'testcafe';
  fixture `test Login`
    .page `http://localhost:8080/`;
  const basic = new Basic();
  test('testLogin', async t => {
    basic.login('karpovrt', '123');
    basic.logout();
  });


