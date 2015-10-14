// This is a Core MIDI hello world app and it has been tested to work
// on OSX Snow Leopard using external MIDI controllers. It creates a
// virtual MIDI source called "My first MIDI source" that you can use
// in e.g. Ableton Live as the input ("MIDI From") for a MIDI
// track. This program captures all notes and other MIDI messages from
// all other MIDI sources and routes them there. Hex dumps of the
// messages are also written to stdout.
//
// Tested on OS X Snow Leopard and XCode 3.2.6.

#include <CoreMIDI/MIDIServices.h>
#include <CoreFoundation/CFRunLoop.h>
#include <stdio.h>

static MIDIClientRef client;
static MIDIPortRef inport;
static MIDIEndpointRef source;

static void MyReadProc(
    const MIDIPacketList *pktlist, void *refCon, void *connRefCon)
{
    (void)refCon;
    (void)connRefCon;
    MIDIReceived(source, pktlist);
    MIDIPacket *packet = (MIDIPacket *)pktlist->packet;
    for (unsigned int j = 0; j < pktlist->numPackets; ++j) {
        for (int i = 0; i < packet->length; ++i) {
            printf("%02X ", packet->data[i]);
        }
        packet = MIDIPacketNext(packet);
    }
    printf("\n");
}

extern int main(void)
{
    MIDIClientCreate(CFSTR("MIDI Echo"), 0, 0, &client);
    MIDIInputPortCreate(client, CFSTR("Input port"), MyReadProc, 0, &inport);
    int n = MIDIGetNumberOfSources();
    printf("%d sources\n", n);
    for (int i = 0; i < n; ++i) {
        MIDIEndpointRef src = MIDIGetSource(i);
        MIDIPortConnectSource(inport, src, 0);
    }
    MIDISourceCreate(client, CFSTR("My first MIDI source"), &source);
    CFRunLoopRun();
    return 0;
}
