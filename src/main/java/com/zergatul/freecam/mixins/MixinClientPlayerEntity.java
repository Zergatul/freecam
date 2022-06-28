package com.zergatul.freecam.mixins;

import com.zergatul.freecam.ModApiWrapper;
import net.minecraft.client.network.ClientPlayerEntity;
import net.minecraft.text.Text;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(ClientPlayerEntity.class)
public abstract class MixinClientPlayerEntity {

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/client/network/ClientPlayerEntity;sendChatMessage(Ljava/lang/String;Lnet/minecraft/text/Text;)V", cancellable = true)
    private void onSendChatMessage(String message, Text preview, CallbackInfo info) {
        ModApiWrapper.Event event = new ModApiWrapper.Event();
        ModApiWrapper.instance.onClientChatEvent(message, event);
        if (event.isCanceled()) {
            info.cancel();
        }
    }
}
