package com.zergatul.freecam.common.mixins;

import com.zergatul.freecam.common.ChatCommandManager;
import net.minecraft.client.multiplayer.ClientPacketListener;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ClientPacketListener.class)
public abstract class MixinClientPacketListener {

    @Inject(at = @At("HEAD"), method = "sendChat(Ljava/lang/String;)V", cancellable = true)
    private void onSendChatMessage(String message, CallbackInfo info) {
        if (ChatCommandManager.instance.handleChatMessage(message)) {
            info.cancel();
        }
    }
}