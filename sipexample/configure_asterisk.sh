#!/bin/bash

# Paths to Asterisk configuration files
SIP_CONF="/etc/asterisk/sip.conf"
EXTENSIONS_CONF="/etc/asterisk/extensions.conf"
BACKUP_DIR="/etc/asterisk/backup"

# Create backup directory
mkdir -p "$BACKUP_DIR"

# Create backups
TIMESTAMP=$(date +%Y%m%d_%H%M%S)
cp "$SIP_CONF" "$BACKUP_DIR/sip.conf.$TIMESTAMP"
cp "$EXTENSIONS_CONF" "$BACKUP_DIR/extensions.conf.$TIMESTAMP"
echo "Backups created: $BACKUP_DIR/sip.conf.$TIMESTAMP and $BACKUP_DIR/extensions.conf.$TIMESTAMP"

# Function to update or append a section in sip.conf
update_sip_section() {
    local section="$1"
    local content="$2"
    local file="$SIP_CONF"
    
    # Check if section exists
    if grep -Fx "$section" "$file" > /dev/null; then
        echo "Section $section already exists in $file, updating content"
        # Remove old section (between [section] and next section or end of file)
        sed -i "/^$section/,/^\[.*\]\|^$/d" "$file"
    else
        echo "Section $section not found in $file, adding new"
    fi
    
    # Append section to file
    echo -e "\n$section\n$content" >> "$file"
}

# Function to update or append a context in extensions.conf
update_extensions_context() {
    local context="$1"
    local content="$2"
    local file="$EXTENSIONS_CONF"
    
    # Check if context exists
    if grep -Fx "$context" "$file" > /dev/null; then
        echo "Context $context already exists in $file, updating content"
        # Remove old context
        sed -i "/^$context/,/^\[.*\]\|^$/d" "$file"
    else
        echo "Context $context not found in $file, adding new"
    fi
    
    # Append context to file
    echo -e "\n$context\n$content" >> "$file"
}

# Configuration for sip.conf
GENERAL_CONFIG="[general]
context=default
bindport=5060
bindaddr=0.0.0.0
transport=udp
allowguest=no
srvlookup=yes"

USER_1000_CONFIG="[1000]
type=friend
context=default
host=dynamic
secret=1234
disallow=all
allow=ulaw
nat=force_rport,comedia"

STREAMER_CONFIG="[streamer]
type=friend
context=default
host=dynamic
secret=1234
disallow=all
allow=ulaw
nat=force_rport,comedia"

# Configuration for extensions.conf
DEFAULT_CONTEXT="[default]
exten => 1000,1,Dial(SIP/1000)
exten => streamer,1,Dial(SIP/streamer)"

# Update sip.conf
update_sip_section "[general]" "$GENERAL_CONFIG"
update_sip_section "[1000]" "$USER_1000_CONFIG"
update_sip_section "[streamer]" "$STREAMER_CONFIG"

# Update extensions.conf
update_extensions_context "[default]" "$DEFAULT_CONTEXT"

# Check syntax
if asterisk -C "$SIP_CONF" -c > /dev/null 2>&1; then
    echo "Syntax check for sip.conf passed"
else
    echo "Syntax error in sip.conf, please check the file"
    exit 1
fi

if asterisk -C "$EXTENSIONS_CONF" -c > /dev/null 2>&1; then
    echo "Syntax check for extensions.conf passed"
else
    echo "Syntax error in extensions.conf, please check the file"
    exit 1
fi

# Restart Asterisk
echo "Restarting Asterisk..."
systemctl restart asterisk
echo "Asterisk restarted"