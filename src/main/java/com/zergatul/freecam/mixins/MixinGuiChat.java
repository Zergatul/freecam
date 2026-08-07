package com.zergatul.freecam.mixins;

import com.zergatul.freecam.ChatCommandManager;
import net.minecraft.client.gui.GuiChat;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(GuiChat.class)
public abstract class MixinGuiChat {

    @Inject(method = "submitChatMessage(Ljava/lang/String;)V", at = @At("HEAD"), cancellable = true, require = 1)
    private void freecam$submitChatMessage(String message, CallbackInfo callback) {
        if (ChatCommandManager.INSTANCE.handleChatMessage(message, true)) {
            callback.cancel();
        }
    }
}