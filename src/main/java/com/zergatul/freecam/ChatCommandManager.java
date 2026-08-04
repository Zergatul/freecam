package com.zergatul.freecam;

import com.zergatul.freecam.ui.FreeCamSettingsScreen;
import net.minecraft.client.Minecraft;

public class ChatCommandManager {

    public static final ChatCommandManager INSTANCE = new ChatCommandManager();

    private int openSettingsScreenTicks = -1;

    private ChatCommandManager() {}

    public boolean handleChatMessage(String message, boolean addToChat) {
        if (message == null || !".freecam".equalsIgnoreCase(message.trim())) {
            return false;
        }

        Minecraft minecraft = Minecraft.getMinecraft();
        if (addToChat) {
            minecraft.ingameGUI.getChatGUI().addToSentMessages(message);
        }

        openSettingsScreenTicks = 4;
        return true;
    }

    public void onClientTickStart() {
        if (openSettingsScreenTicks > 0 && --openSettingsScreenTicks == 0) {
            openSettingsScreenTicks = -1;
            Minecraft.getMinecraft().displayGuiScreen(new FreeCamSettingsScreen());
        }
    }
}