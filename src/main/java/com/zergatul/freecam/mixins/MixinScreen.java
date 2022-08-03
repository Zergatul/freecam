package com.zergatul.freecam.mixins;

import com.zergatul.freecam.ModApiWrapper;
import net.minecraft.client.gui.screen.Screen;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(Screen.class)
public abstract class MixinScreen {

    @Inject(at = @At(value = "INVOKE", target = "Lnet/minecraft/client/network/ClientPlayerEntity;sendChatMessage(Ljava/lang/String;)V"), method = "Lnet/minecraft/client/gui/screen/Screen;sendMessage(Ljava/lang/String;Z)V", cancellable = true)
    private void onSendMessage(String message, boolean toHud, CallbackInfo info) {
        ModApiWrapper.Event event = new ModApiWrapper.Event();
        ModApiWrapper.instance.onClientChatEvent(message, event);
        if (event.isCanceled()) {
            info.cancel();
        }
    }
}