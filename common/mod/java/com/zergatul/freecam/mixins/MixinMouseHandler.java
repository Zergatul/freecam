package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import com.zergatul.mixin.WrapMethodInsideIfCondition;
import net.minecraft.client.MouseHandler;
import net.minecraft.client.player.LocalPlayer;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;

// inject with priority 100 to be before other mod that can redirect
@Mixin(value = MouseHandler.class, priority = 100)
public abstract class MixinMouseHandler {

    @WrapMethodInsideIfCondition(method = "turnPlayer", at = @At(value = "INVOKE", target = "Lnet/minecraft/client/player/LocalPlayer;turn(DD)V"))
    private static boolean onBeforeCallPlayerTurn(LocalPlayer player, double yRot, double xRot) {
        return FreeCam.INSTANCE.onPlayerTurn(yRot, xRot);
    }
}