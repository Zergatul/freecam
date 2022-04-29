package com.zergatul.freecam.mixins;

import com.mojang.authlib.GameProfile;
import com.zergatul.freecam.FreeCamController;
import net.minecraft.client.multiplayer.ClientPacketListener;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

@Mixin(ClientPacketListener.class)
public class MixinClientPacketListener {

    @Inject(at = @At("HEAD"), method = "Lnet/minecraft/client/multiplayer/ClientPacketListener;getLocalGameProfile()Lcom/mojang/authlib/GameProfile;", cancellable = true)
    private void onGetLocalGameProfile(CallbackInfoReturnable<GameProfile> cir) {
        if (FreeCamController.instance.profileOverride != null) {
            cir.setReturnValue(FreeCamController.instance.profileOverride);
            cir.cancel();
            return;
        }
    }
}
