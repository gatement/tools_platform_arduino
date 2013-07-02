#include <SPI.h>
#include <Ethernet.h>
#include <PubSubClient.h>
#include "debug.h"
#include "settings.h"

long previousMillis = 0;
EthernetClient ethClient;
PubSubClient client(server, port, callback, ethClient);

void callback(char* topic, byte* payload, unsigned int length) {
  if(payload[0] == CMD_SWITCH_CONTROL)
  {
    switch(payload[1])
    {
      case 1:
        myDigitalWrite(switch1, payload[2]);
        break;
      case 2:
        myDigitalWrite(switch2, payload[2]);
        break;
      case 3:
        myDigitalWrite(switch3, payload[2]);
        break;
    } 
    send_switch_status();
  }
}

void setup()
{
  DBG("--------- MY ETHERNET DEVICE --------");
  Ethernet.begin(mac);
  Serial.begin(9600);
  
  pinMode(switch1, OUTPUT);
  pinMode(switch2, OUTPUT);
  pinMode(switch3, OUTPUT);
  myDigitalWrite(switch1, LOW);
  myDigitalWrite(switch2, LOW);
  myDigitalWrite(switch3, LOW);

  Ethernet.begin(mac, ip);
  connect();
}

void loop()
{  
  if(client.loop())
  {
    my_loop();
  }
  else
  {
    delay(RECONNECT_TIMEOUT);
    connect();    
  }
}

void my_loop()
{
  unsigned long currentMillis = millis(); 
  if(currentMillis - previousMillis > sendStatusInterval) 
  {
    previousMillis = currentMillis;    
    send_switch_status();
  }
}

void connect()
{
  DBG("connecting...");
  if(client.connect(deviceId, username, password))
  {
    DBG("   connected");
  }
}

void send_switch_status()
{
    int switchStatus = myDigitalRead(switch1) + myDigitalRead(switch2) * 2 + myDigitalRead(switch3) * 4;
    DBG0("status: ");
    DBG(switchStatus);
    uint8_t payload[] = {CMD_SWITCH_STATUS, switchStatus};
    client.publish(switchStatusTopic, payload, 2);
}

unsigned char myDigitalRead(unsigned char switchId)
{
  unsigned char val = digitalRead(switchId);
  unsigned char val2 = val == HIGH? LOW : HIGH;
  return val2;
}

void myDigitalWrite(unsigned char switchId, byte val)
{
  unsigned char val2 = val == HIGH? LOW : HIGH;
  digitalWrite(switchId, val2);
}
