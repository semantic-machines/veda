import Basic from './basic';
import config from './config';
fixture `test Login`
  .page `${config.baseUrl}`;
const basic = new Basic();
test('testLogin', async () => {
  const username = 'karpovrt';
  const password = '123';
  
  console.log(`Шаг 1: Login as ${username}`);
  basic.login(username, password);
  
  console.log(`Шаг 2: Logout user ${username}`);
  basic.logout();
  
  console.log('Тест testLogin завершён');
});
