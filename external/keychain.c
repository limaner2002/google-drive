#include <CoreFoundation/CoreFoundation.h>
#include <Security/Security.h>
#include <CoreServices/CoreServices.h>
#include <stdio.h>

int StorePasswordKeychain (void* serviceName, UInt32 serviceNameLength,
			   void* accountName, UInt32 accountNameLength,
			   void* password, UInt32 passwordLength
			   ){
  assert(passwordLength <= 0xffffffff);
  OSStatus status;
  status = SecKeychainAddGenericPassword (
  					  NULL,               // default keychain
  					  serviceNameLength,                 // length of service name
  					  serviceName,  // service name
  					  accountNameLength,                 // length of account name
  					  accountName,          // account name
  					  passwordLength,     // length of password
  					  password,           // pointer to password data
  					  NULL                // the item reference
  					  );
  return status;
}
