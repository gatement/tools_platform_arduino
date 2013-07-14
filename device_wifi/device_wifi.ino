#include <Arduino.h>
#include <SoftwareSerial.h>
#include <WiFly.h>
#include "debug.h"
#include "settings.h"

byte mac[20];
unsigned int macLen = 0;

unsigned int i, j;

char recv_buffer[RECV_BUFFER_LEN];
unsigned int byteCount;

WiFly wifly(2, 3); //wifly(TX, RX)

void setup() {
  Serial.begin(9600);

  pinMode(led1, OUTPUT);
  
  DBG("--------- MY WIFLY DEVICE --------");
  
  // wait for initilization of wifly
  delay(3000);
  
  while(!initWifi())
  {
    delay(5000);
  }
  
  digitalWrite(led1, HIGH);
  sendLed1Status();
}

void loop() {    
  byteCount = wifly.receive((uint8_t *)recv_buffer, RECV_BUFFER_LEN, 50);  
  if(byteCount > 0)
  {
    recv_buffer[byteCount] = '\0';
    String receivedStr = recv_buffer;    
    DBG0("received: ");
    DBG(receivedStr);
    
    // led1 remote control
    if(receivedStr.indexOf("#1#") != -1)
    {
      char val = receivedStr.charAt(receivedStr.indexOf("#1#") + 3);
      controlLed1(val);
    }
    
    // re-connect TCP
    if(receivedStr.indexOf("*CLOS*") != -1)
    {
      delay(RETRY_HOST_TIMEOUT);
      while(!connectHost());
    }
  } 
    
}

void controlLed1(char val)
{    
    if(val == 1)
    {
      digitalWrite(led1, HIGH);
      DBG("led1: on");
    }
    else
    {
      digitalWrite(led1, LOW);
      DBG("led1: off");
    }
    
    sendLed1Status();
}

void sendLed1Status()
{
    // send led status to remote
    wifly.write('C');
    wifly.write(digitalRead(led1));
}

boolean initWifi()
{ 
  wifly.reset();
  delay(1000);
  wifly.sendCommand("set wlan join 0\r");  
  
  macLen = getMac(mac); 
  printMac();
  
  if(!joinAP())
  {
    return false;
  }
  
  if(!connectHost())
  {
    return false;
  }
  
  // back to data mode
  while(!wifly.dataMode())
  {
      delay(1000);
  }
  
  DBG("init done.");
  
  return true;
}

boolean joinAP()
{ 
  DBG("joining: " SSID);
  int retry_times = 0;
  while(!wifly.join(SSID, PASSWORD, AUTH))
  {
    DBG("  retrying...");
    retry_times++;
    if(retry_times == 10)
    {
      break;
    }
    delay(RETRY_WIFI_TIMEOUT);
  }
  if(retry_times == 10)
  {
    DBG("  too many retry so give up.");
    return false;
  }
  else
  {
    DBG("  joined: " SSID );
    return true;
  }
}

boolean connectHost()
{  
  DBG("connecting: " HOST);
  int retry_times = 0;
  while(!wifly.connect(HOST, PORT, CONNECT_TIMEOUT))
  {
    DBG("  retrying...");
    retry_times++;
    if(retry_times == 10)
    {
      break;
    }
    delay(RETRY_HOST_TIMEOUT);
  }
  
  if(retry_times == 10)
  {
    DBG("  too many retry so give up.");
    return false;
  }
  else
  {
    DBG("  connected: " HOST );
    sendOnlineData();
    return true;
  }
}

void sendOnlineData()
{
  wifly.write('A');

  for(i=0; i < macLen; i++)
  {
    wifly.write(mac[i]);
  }
}

int getMac(byte *mac)
{ 
  int macLen = 0;
  
  wifly.clear();
  
  wifly.sendCommand("get mac\r");
  byteCount = wifly.receive((uint8_t *)recv_buffer, RECV_BUFFER_LEN, 5000);
  if(byteCount > 0)
  {
    recv_buffer[byteCount] = '\0';
    String receivedStr = recv_buffer;
    
    receivedStr = receivedStr.substring(receivedStr.indexOf('=') + 1, receivedStr.indexOf('=') + 18);
    receivedStr.toUpperCase();
    int len = receivedStr.length() + 1;    
    byte temp[len];
    receivedStr.getBytes(temp, len);
    
    // cut the colons (":") in the mac
    j = 0;
    for(i = 0;i < len; i++)
    {
      if(temp[i] != 0x3A)
      {
        mac[j] = temp[i];
        j++;
      }
    }
    macLen = j - 1;
  }  
  
  return macLen;
}

void printMac()
{
  DBG0("mac: ");
  for(i = 0;i < macLen; i++)
  {
    DBG0((char)mac[i]);
  }
  DBG("");
}
