package com.zergatul.freecam.mixins;

import com.zergatul.freecam.ChatCommandManager;
import com.zergatul.freecam.FreeCam;
import net.minecraft.client.multiplayer.ClientPacketListener;
import net.minecraft.network.protocol.common.custom.CustomPacketPayload;
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

    @Inject(at = @At("HEAD"), method = "handleCustomPayload(Lnet/minecraft/network/protocol/common/custom/CustomPacketPayload;)V")
    private void onCustomPayload(CustomPacketPayload payload, CallbackInfo info) {
        if (FreeCam.SERVER_DISABLE_CHANNEL.equals(payload.type().id().toString())) {
            FreeCam.instance.disableOnServer();
        }
    }
}
