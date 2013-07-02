
#define RECONNECT_TIMEOUT 20000
#define CMD_SWITCH_STATUS 3
#define CMD_SWITCH_CONTROL 4

byte mac[]    = {0x00, 0x00, 0x00, 0x00, 0x00, 0x02};
char* deviceId = "000000000002";
char* switchStatusTopic = "/000000000002/switch_status";
byte ip[]     = { 192, 168, 1, 12 };
byte server[] = { 192, 168, 1, 10 };
uint16_t port = 1883;
char* username = "admin";
char* password = "admin";

unsigned char switch1 = 2;
unsigned char switch2 = 3;
unsigned char switch3 = 5;

long sendStatusInterval = 60000;  

