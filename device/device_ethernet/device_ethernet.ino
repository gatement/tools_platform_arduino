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

// Update these with values suitable for your network.
byte mac[]    = {  0x00, 0x00, 0x00, 0x00, 0x00, 0x02 };
byte server[] = { 192, 168, 1, 11 };
byte ip[]     = { 192, 168, 1, 12 };

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
  if (client.connect("000000000002", "admin", "admin")) {
    //client.publish("outTopic","hello world");
    //client.subscribe("inTopic");
  }
}

void loop()
{
  client.loop();
}

