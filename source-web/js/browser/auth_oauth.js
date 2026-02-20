/**
 * @module OAuth Authentication
 */
let stateSending;
let authWindowRef = null; // Сохраняем ссылку на окно

/**
 * Создает URL для OAuth авторизации
 * @return {string} URL для авторизации
 */
function createOAuthUrl() {
  const authUrl = 'https://online.saby.ru/oauth/api/token';
  const clientId = '0548008610228640';

  const redirectUri = `${window.location.origin}/ideas/#/oauth/callback`;
  
  // Генерируем state для защиты от CSRF атак
  stateSending = Math.random().toString(36).substring(2, 15) + Math.random().toString(36).substring(2, 15);
  sessionStorage.setItem('oauth_state', stateSending);

  const params = {
    client_id: clientId,
    response_type: 'token',
    scope: 'openid',
    redirect_uri: redirectUri,
    state: stateSending
  };
  return `${authUrl}?${Object.entries(params).map(([key, value]) => `${key}=${value}`).join('&')}`;
}

/**
 * Открывает окно OAuth авторизации и ждет получения токена
 * @return {Promise<Object>} Promise, который разрешается с объектом, содержащим access_token и другие параметры
 */
export async function sabyAuthWindow(windowRef = null){
  const authWindow = windowRef || authWindowRef;
  if (!authWindow || authWindow.closed) {
    throw new Error('Окно авторизации не было открыто или было закрыто');
  }

  // Возвращаем Promise, который разрешится при получении токена или ошибки
  return new Promise((resolve, reject) => {
    let checkClosed;
    
    // Слушаем сообщения от popup окна
    const messageHandler = (event) => {
      // Проверяем origin для безопасности
      if (event.origin !== window.location.origin) {
        return;
      }
      
      if (event.data && event.data.type === 'oauth_callback') {
        // Очищаем обработчики
        window.removeEventListener('message', messageHandler);
        if (checkClosed) clearInterval(checkClosed);
        
        sessionStorage.removeItem('oauth_state');
        
        // Проверяем наличие ошибки
        if (event.data.error) {
          reject(new Error(`Ошибка OAuth: ${event.data.error}`));
          return;
        }
        
        // Для Implicit Flow получаем access_token
        const accessToken = event.data.access_token;
        if (!accessToken) {
          reject(new Error('Access token не получен от провайдера OAuth'));
          return;
        }
        
        // Возвращаем токен для дальнейшей обработки
        resolve({
          access_token: accessToken,
          token_type: event.data.token_type || 'Bearer',
          expires_in: event.data.expires_in,
          state: event.data.state
        });
      }
    };
    
    window.addEventListener('message', messageHandler);
    
    // Проверяем, не закрыл ли пользователь окно
    checkClosed = setInterval(() => {
      if (authWindow.closed) {
        console.log('Окно авторизации было закрыто');
        clearInterval(checkClosed);
        window.removeEventListener('message', messageHandler);
        sessionStorage.removeItem('oauth_state');
        reject(new Error('Окно авторизации было закрыто'));
      }
    }, 1000);
  });
}

/**
 * Обрабатывает OAuth callback из popup окна
 * Извлекает access_token из hash и отправляет его родительскому окну через postMessage
 * @return {void}
 */
export function handleOAuthCallback() {
  const hash = window.location.hash;
  
  // Проверяем наличие access_token или error в hash
  if (!hash || (!hash.includes('access_token=') && !hash.includes('error='))) {
    return;
  }
  
  // Парсим hash - убираем начальный # и /oauth/callback если есть
  let hashPart = hash.replace(/^#/, '');
  if (hashPart.includes('/oauth/callback')) {
    hashPart = hashPart.replace(/^\/oauth\/callback/, '');
  }
  
  // Извлекаем параметры из hash
  let accessToken, tokenType, expiresIn, state, error, errorDescription;
  
  if (hashPart && hashPart.includes('=')) {
    const paramsString = hashPart.startsWith('?') ? hashPart.substring(1) : hashPart;
    const hashParams = new URLSearchParams(paramsString);
    
    accessToken = hashParams.get('access_token');
    tokenType = hashParams.get('token_type');
    expiresIn = hashParams.get('expires_in');
    state = hashParams.get('state');
    error = hashParams.get('error');
    errorDescription = hashParams.get('error_description');
  }
  
  if (!accessToken && !error) {
    return;
  }
  
  // Отправляем сообщение родительскому окну (если это popup)
  if (window.opener && !window.opener.closed) {
    window.opener.postMessage({
      type: 'oauth_callback',
      access_token: accessToken,
      token_type: tokenType,
      expires_in: expiresIn ? parseInt(expiresIn) : null,
      state: state,
      error: error,
      error_description: errorDescription
    }, window.location.origin);
    
    // Закрываем popup после отправки сообщения
    setTimeout(() => {
      if (!window.closed) {
        window.close();
      }
    }, 50);
  }
}

/**
 * Инициализирует OAuth авторизацию для формы входа
 * @param {HTMLElement} loginForm - Форма входа
 * @param {Function} handleLoginSuccess - Функция обработки успешной авторизации
 * @param {Function} handleLoginError - Функция обработки ошибок авторизации
 * @param {Object} Backend - Backend объект для вызова API
 * @param {Function} spinnerDecorator - Декоратор для показа спиннера
 * @param {Function} delegateHandler - Функция для делегирования обработчиков событий
 * @return {void}
 */
// export function initOAuth(loginForm, handleLoginSuccess, handleLoginError, spinnerDecorator, delegateHandler) {
//   delegateHandler(loginForm, 'click', '#saby-auth', spinnerDecorator(async function (e) {
//     e.preventDefault();
//     try {
//       await OAuthAuthentication(handleLoginSuccess);
//     } catch (error) {
//       console.error('Ошибка OAuth авторизации:', error);
//       await handleLoginError(error);
//     }
//   }));
// }

export async function OAuthAuthentication() {
  if (window.location.pathname.indexOf('/ideas/') < 0) {
    return Promise.reject(new Error('OAuth authentication is not allowed on this page'));
  }
  const url = createOAuthUrl();
  authWindowRef = window.open(
    url,
    "Подключение к СБИС",
    "width=600,height=700,scrollbars=1,toolbar=0,resizable=1"
  );
  if (!authWindowRef) {
    throw new Error('Не удалось открыть окно авторизации. Проверьте настройки блокировки всплывающих окон в браузере.');
    // handleLoginError(new Error('Не удалось открыть окно авторизации. Проверьте настройки блокировки всплывающих окон в браузере.'));
    // return;
  }

  const oauthResult = await sabyAuthWindow();
  console.log(`token: ${oauthResult.access_token}`);
  const authResult = await authenticateViaSbis(oauthResult.access_token);
  // Обработка успешной аутентификации
  if (authResult && authResult.id && authResult.user_uri) {
    return {
      ticket: authResult.id,
      user_uri: authResult.user_uri,
      end_time: Math.floor((authResult.end_time - 621355968000000000) / 10000),
    }
  } else {
    console.log(`Authentication failed: ${authResult}`);
    throw new Error('Неверный ответ сервера');
  }
}

async function authenticateViaSbis(sbisAccessToken) {
  const response = await fetch('/auth/sbis/authenticate', {
    method: 'POST',
    headers: {
      'Content-Type': 'application/json'
    },
    body: JSON.stringify({
      access_token: sbisAccessToken
    })
  });
  console.log(`response: ${JSON.stringify(response)}`);
  if (!response.ok) {
    throw new Error(`Authentication failed: ${response.status}`);
  }

  return await response.json();
}
