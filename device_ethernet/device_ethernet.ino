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

byte mac[]    = {  0x00, 0x00, 0x00, 0x00, 0x00, 0x02 };
char* deviceId = "000000000002";
byte ip[]     = { 192, 168, 1, 12 };
byte server[] = { 192, 168, 1, 11 };
int port = 1833;
char* username = "admin";
char* password = "admin";

void callback(char* topic, byte* payload, unsigned int length) {
  // handle message arrived
}

EthernetClient ethClient;
PubSubClient client(server, 1883, callback, ethClient);

void setup()
{
  DBG("--------- MY ETHERNET DEVICE --------");
  Ethernet.begin(mac);
  Serial.begin(9600);

  Ethernet.begin(mac, ip);
  connect();
}

void loop()
{
  DBG(client.loop());
  if(!client.loop())
  {
    delay(RECONNECT_TIMEOUT);
    connect();
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

