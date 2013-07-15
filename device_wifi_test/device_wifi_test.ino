#include <Arduino.h>
#include <SoftwareSerial.h>
#include <WiFly.h>

#define SSID      "ssid"
#define KEY       "pwd"
#define AUTH      WIFLY_AUTH_WPA2_PSK

WiFly wifly(2, 3); //wifly(TX, RX)

void setup() {
  Serial.begin(9600);  
  Serial.println("--------- MY WIFLY DEVICE --------");
  
}

void loop() {
}

