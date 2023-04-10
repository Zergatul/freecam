package com.zergatul.freecam.common.mixins;

import com.zergatul.freecam.common.ChatCommandManager;
import net.minecraft.client.player.LocalPlayer;
import net.minecraft.network.chat.Component;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(LocalPlayer.class)
public abstract class MixinLocalPlayer {

    @Inject(at = @At("HEAD"), method = "chatSigned(Ljava/lang/String;Lnet/minecraft/network/chat/Component;)V", cancellable = true)
    private void onChatSigned(String message, Component preview, CallbackInfo info) {
        if (ChatCommandManager.instance.handleChatMessage(message)) {
            info.cancel();
        }
    }
}