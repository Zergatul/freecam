package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import net.minecraft.client.Mouse;
import net.minecraft.client.network.ClientPlayerEntity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Redirect;

@Mixin(Mouse.class)
public abstract class MixinMouse {

    @Redirect(
            method = "updateMouse()V",
            at = @At(value = "INVOKE", target = "Lnet/minecraft/client/network/ClientPlayerEntity;changeLookDirection(DD)V"))
    private void onLocalPlayerTurn(ClientPlayerEntity player, double yRot, double xRot) {
        FreeCamController.instance.onPlayerTurn(player, yRot, xRot);
    }
}