package com.zergatul.freecam.mixins;

import com.zergatul.freecam.ChatCommandManager;
import net.minecraft.client.gui.GuiScreen;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(GuiScreen.class)
public abstract class MixinGuiScreen {

    @Inject(at = @At("HEAD"), method = "sendChatMessage(Ljava/lang/String;Z)V", cancellable = true)
    private void onSendChatMessage(String message, boolean addToChat, CallbackInfo info) {
        if (ChatCommandManager.INSTANCE.handleChatMessage(message, addToChat)) {
            info.cancel();
        }
    }
}