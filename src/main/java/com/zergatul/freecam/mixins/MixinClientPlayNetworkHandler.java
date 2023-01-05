package com.zergatul.freecam.mixins;

import com.zergatul.freecam.ModApiWrapper;
import net.minecraft.client.network.ClientPlayNetworkHandler;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ClientPlayNetworkHandler.class)
public abstract class MixinClientPlayNetworkHandler {

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/client/network/ClientPlayNetworkHandler;sendChatMessage(Ljava/lang/String;)V", cancellable = true)
    private void onSendChatMessage(String content, CallbackInfo info) {
        ModApiWrapper.Event event = new ModApiWrapper.Event();
        ModApiWrapper.instance.onClientChatEvent(content, event);
        if (event.isCanceled()) {
            info.cancel();
        }
    }
}