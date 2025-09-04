

// SMS Authentication Configuration
const SMS_CONFIG = {
  client_secret: 'your-client-secret-here', // TODO: получить из конфигурации
  api_base_url: window.location.origin,
  max_retries: 3,
  timeout: 30000
};

export async function getMobileCode (event, loginForm) {
  event.preventDefault();
  const phoneInput = loginForm.querySelector('#mobile-login');
  const phone = phoneInput.value.trim();
  
  try {
    // Нормализация номера телефона
    const normalizedPhone = normalizePhone(phone);
    
    // Генерация параметров для подписи
    const timestamp = Math.floor(Date.now() / 1000);
    const nonce = generateUUID();
    const salt = generateSalt();
    
    // Генерация HMAC подписи
    const signature = await generateSmsSignature(normalizedPhone, timestamp, nonce, salt);
    
    // Формирование запроса
    const requestData = {
      phone: normalizedPhone,
      timestamp: timestamp,
      nonce: nonce,
      salt: salt,
      signature: signature
    };
    
    console.log('SMS Request data:', requestData);
    
    // Отправка запроса
    const response = await fetch(`${SMS_CONFIG.api_base_url}/auth/sms/request`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'X-Client-Type': 'web-app'
      },
      body: JSON.stringify(requestData)
    });
    
    if (!response.ok) {
      throw new Error(`HTTP ${response.status}: ${response.statusText}`);
    }
    return await response.json();
  } catch (error) {
    console.error('SMS request error:', error);
    showSmsErrorMessage(error.message);
  }
}

export async function verifySmsCode (event, loginForm) {
  event.preventDefault();
  
  const codeInput = loginForm.querySelector('#sms-code');
  const code = codeInput.value.trim();
  
  if (!code || code.length !== 6) {
    showSmsErrorMessage('Введите 6-значный код из SMS');
    return;
  }
  
  const token = sessionStorage.getItem('sms_auth_token');
  if (!token) {
    showSmsErrorMessage('Сессия истекла. Запросите код повторно.');
    return;
  }
  
  try {
    const requestData = {
      token: token,
      code: code
    };
    
    console.log('SMS Verify data:', requestData);
    
    const response = await fetch(`${SMS_CONFIG.api_base_url}/auth/sms/verify`, {
      method: 'POST',
      headers: {
        'Content-Type': 'application/json',
        'X-Client-Type': 'web-app'
      },
      body: JSON.stringify(requestData)
    });
    
    if (!response.ok) {
      if (response.status === 473) {
        throw new Error('Неверный код. Попробуйте еще раз.');
      }
      throw new Error(`HTTP ${response.status}: ${response.statusText}`);
    }
    
    return await response.json();
  } catch (error) {
    console.error('SMS verify error:', error);
    showSmsErrorMessage(error.message);
    codeInput.value = ''; // Очистить поле ввода
  }
}
/**
 * Generate random salt (16 bytes, hex encoded)
 * @return {string} - hex encoded salt
 */
function generateSalt() {
  const array = new Uint8Array(16);
  crypto.getRandomValues(array);
  return Array.from(array, byte => byte.toString(16).padStart(2, '0')).join('');
}

/**
 * Generate UUID v4
 * @return {string} - UUID v4 string
 */
function generateUUID() {
  return crypto.randomUUID();
}

/**
 * Compute HMAC-SHA256 signature
 * @param {string} message - message to sign
 * @param {string} key - signing key
 * @return {string} - hex encoded signature
 */
function computeHmac(message, key) {
  // Convert key to ArrayBuffer
  const keyBuffer = new TextEncoder().encode(key);
  
  // Import key for HMAC
  return crypto.subtle.importKey(
    'raw',
    keyBuffer,
    { name: 'HMAC', hash: 'SHA-256' },
    false,
    ['sign']
  ).then(cryptoKey => {
    // Sign the message
    return crypto.subtle.sign('HMAC', cryptoKey, new TextEncoder().encode(message));
  }).then(signatureBuffer => {
    // Convert to hex string
    const signatureArray = new Uint8Array(signatureBuffer);
    return Array.from(signatureArray, byte => byte.toString(16).padStart(2, '0')).join('');
  });
}

/**
 * Normalize phone number to format: 79991234567
 * @param {string} phone - input phone number
 * @return {string} - normalized phone number
 */
function normalizePhone(phone) {
  // Remove all non-digit characters
  const digits = phone.replace(/\D/g, '');
  
  // Handle different formats
  if (digits.startsWith('8') && digits.length === 11) {
    // 8 999 123 45 67 -> 79991234567
    return '7' + digits.substring(1);
  } else if (digits.startsWith('7') && digits.length === 11) {
    // 7 999 123 45 67 -> 79991234567
    return digits;
  } else if (digits.length === 10) {
    // 999 123 45 67 -> 79991234567
    return '7' + digits;
  }
  
  throw new Error('Неверный формат номера телефона');
}

/**
 * Generate HMAC signature for SMS request
 * @param {string} phone - normalized phone number
 * @param {string} timestamp - unix timestamp
 * @param {string} nonce - UUID v4
 * @param {string} salt - random salt
 * @return {Promise<string>} - hex encoded signature
 */
async function generateSmsSignature(phone, timestamp, nonce, salt) {
  const message = `action=sms_request|nonce=${nonce}|phone=${phone}|salt=${salt}|timestamp=${timestamp}`;
  const key = SMS_CONFIG.client_secret + salt;
  return await computeHmac(message, key);
}

/**
 * Show SMS code input form
 * @return {void}
 */
function showSmsCodeForm() {
  const mobileLoginSection = loginForm.querySelector('#mobile-login-section');
  const smsCodeSection = loginForm.querySelector('#sms-code-section');
  
  if (mobileLoginSection) hide(mobileLoginSection);
  if (smsCodeSection) show(smsCodeSection);
}

/**
 * Show SMS success message
 * @return {void}
 */
function showSmsSuccessMessage(message) {
  // const smsSuccessMessage = loginForm.querySelector('#sms-success-message');
  // if (smsSuccessMessage) {
  //   show(smsSuccessMessage);
  //   // Hide after 5 seconds
  //   setTimeout(() => hide(smsSuccessMessage), 5000);
  // }
  console.log(`showSmsSuccessMessage : ${message}`);
}

/**
 * Show SMS error message
 * @param {string} message - error message
 * @return {void}
 */
function showSmsErrorMessage(message) {
  console.log(`showSmsErrorMessage : ${message}`);
  // const smsErrorMessage = loginForm.querySelector('#sms-error-message');
  // if (smsErrorMessage) {
  //   smsErrorMessage.textContent = message;
  //   show(smsErrorMessage);
  //   // Hide after 10 seconds
  //   setTimeout(() => hide(smsErrorMessage), 10000);
  // }
}