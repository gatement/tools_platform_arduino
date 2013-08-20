#include <SPI.h>
#include <Ethernet.h>
#include <PubSubClient.h>
#include "debug.h"

#define RECONNECT_TIMEOUT 20000
#define CMD_SWITCH_STATUS 3
#define CMD_SWITCH_CONTROL 4
#define CMD_HUMAN_STATUS 7

byte mac[]    = {0x00, 0x00, 0x00, 0x00, 0x00, 0x02};
char* deviceId = "000000000002";
char* switchStatusTopic = "/000000000002/status";
char* humanStatusTopic = "/000000000002/status";
byte ip[]     = { 192, 168, 1, 12 };
byte server[] = { 192, 168, 1, 10 };
uint16_t port = 1883;
char* username = "usr";
char* password = "pwd";

unsigned char switch1 = 2;
unsigned char switch2 = 3;
unsigned char switch3 = 5;
unsigned char humanSensor = 7;

long sendStatusInterval = 60000;  

unsigned char mySwitch2Status = 0;
long previousMillis = 0;
int previousHumanStatus = -1;
EthernetClient ethClient;
PubSubClient pubSubClient(server, port, callback, ethClient);

void callback(char* topic, byte* payload, unsigned int length) {
	if(payload[0] == CMD_SWITCH_CONTROL)
	{
		switch(payload[1])
		{
			case 1:
				myDigitalWrite(switch1, payload[2]);
				break;
			case 2:
				digitalWrite(switch2, payload[2]);
				mySwitch2Status = payload[2];
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
	Serial.begin(9600);
	dbgln("--------- MY ETHERNET DEVICE --------");

	pinMode(switch1, OUTPUT);
	pinMode(switch2, OUTPUT);
	pinMode(switch3, OUTPUT);
	pinMode(humanSensor, INPUT);

	myDigitalWrite(switch1, HIGH);
	digitalWrite(switch2, LOW);
	myDigitalWrite(switch3, LOW);

	Ethernet.begin(mac, ip);
	connect();
}

void loop()
{  
	if(pubSubClient.loop())
	{
		my_loop();
	}
	else
	{
		delay(RECONNECT_TIMEOUT);
		connect();  
	}

	human_status();
}

void human_status()
{
	int humanStatus = digitalRead(humanSensor);

	unsigned char oldSwitch2Val = digitalRead(switch2);
	if(oldSwitch2Val != humanStatus && mySwitch2Status == 0)
	{
		digitalWrite(switch2, humanStatus);
		send_switch_status();
	}

	if(humanStatus != previousHumanStatus)
	{
		if(humanStatus == HIGH)
		{
			send_human_status(humanStatus);		
		}
		previousHumanStatus = humanStatus;
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
	dbgln("connecting...");
	if(pubSubClient.connect(deviceId, username, password))
	{
		dbgln("   connected");
		send_switch_status();
	}
}

void send_switch_status()
{
	int switchStatus = myDigitalRead(switch1) + digitalRead(switch2) * 2 + myDigitalRead(switch3) * 4;
	dbg("switch status: ");
	dbgln(switchStatus);
	uint8_t payload[] = {CMD_SWITCH_STATUS, switchStatus};
	pubSubClient.publish(switchStatusTopic, payload, 2);
}

void send_human_status(int humanStatus)
{
	dbg("human status: ");
	dbgln(humanStatus);
	uint8_t payload[] = {CMD_HUMAN_STATUS, humanStatus};
	pubSubClient.publish(humanStatusTopic, payload, 2);
}

unsigned char myDigitalRead(unsigned char switchId)
{
	unsigned char val = digitalRead(switchId);
	unsigned char val2 = val == HIGH ? LOW : HIGH;
	return val2;
}

void myDigitalWrite(unsigned char switchId, byte val)
{
	unsigned char val2 = val == HIGH ? LOW : HIGH;
	digitalWrite(switchId, val2);
}

