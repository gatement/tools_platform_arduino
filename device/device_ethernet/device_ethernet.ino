#include <Ethernet.h>
#include <SPI.h>

#define DEBUG // comment me out to disable Debug info
#ifdef DEBUG
#define DBG0(message)    Serial.print(message)
#define DBG(message)    Serial.println(message)
#else
#define DBG0(message)
#define DBG(message)
#endif // DEBUG

#define RETRY_HOST_TIMEOUT      10000

#define HOST      "johnson.uicp.net"
#define PORT      10000

byte mac[] = { 0x41, 0x42,  0x41, 0x30,  0x41, 0x31 };
char online_cmd[] = "AEFA1A2A3A4FE";
byte host[] = {192,168,1,11};

EthernetClient client;

void setup()
{
  DBG("--------- MY ETHERNET DEVICE --------");
  
  Ethernet.begin(mac);
  Serial.begin(9600);
  
  // wait for initilization of ethernet shield
  delay(3000);

  Serial.println("connecting...");
  while (!client.connect(host, PORT)) {
    Serial.println("  retrying");
    delay(5000);
  }  
  Serial.println("  connected");
  
  Serial.println("sending online data...");
  sendOnlineData();
  Serial.println("  sent");
   
  //test
  delay(3000);
  sendVoltageData();
}

void loop()
{
  if (client.available()) {
    char c = client.read();
    Serial.print(c);
  }

  if (!client.connected()) {
    Serial.println();
    Serial.println("disconnecting.");
    client.stop();
    for(;;)
      ;
  }
}

void sendOnlineData()
{
  client.print(online_cmd);
}

void sendVoltageData()
{  
  client.print("B  ");
}
