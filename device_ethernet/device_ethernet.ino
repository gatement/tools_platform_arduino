#include <SPI.h>
#include <Ethernet.h>
#include <PubSubClient.h>

#define DEBUG // comment me out to disable Debug info
#ifdef DEBUG
#define DBG0(message)    Serial.print(message)
#define DBG(message)    Serial.println(message)
#else
#define DBG0(message)
#define DBG(message)
#endif // DEBUG

#define RECONNECT_TIMEOUT 10000
#define CMD_SWITCH_STATUS 3
#define CMD_SWITCH_CONTROL 4

byte mac[]    = {  0x00, 0x00, 0x00, 0x00, 0x00, 0x02 };
char* deviceId = "000000000002";
char* switchStatusTopic = "/000000000002/switch_status";
byte ip[]     = { 192, 168, 1, 12 };
byte server[] = { 192, 168, 1, 11 };
int port = 1833;
char* username = "admin";
char* password = "admin";

unsigned char switch1 = 2;
unsigned char switch2 = 3;
unsigned char switch3 = 5;

long previousMillis = 0;
long sendStatusInterval = 10000;  

EthernetClient ethClient;
PubSubClient client(server, 1883, callback, ethClient);

void callback(char* topic, byte* payload, unsigned int length) {
  if(payload[0] == CMD_SWITCH_CONTROL)
  {
    switch(payload[1])
    {
      case 1:
        digitalWrite(switch1, payload[2]);
        break;
      case 2:
        digitalWrite(switch2, payload[2]);
        break;
      case 3:
        digitalWrite(switch3, payload[2]);
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
  digitalWrite(switch1, LOW);
  digitalWrite(switch2, LOW);
  digitalWrite(switch3, LOW);

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
    int switchStatus = digitalRead(switch1) + digitalRead(switch2) * 2 + digitalRead(switch3) * 4;
    DBG0("status: ");
    DBG(switchStatus);
    uint8_t payload[] = {CMD_SWITCH_STATUS, switchStatus};
    client.publish(switchStatusTopic, payload, 2);
}

