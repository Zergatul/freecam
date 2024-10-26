package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCam;
import com.zergatul.mixin.ModifyMethodReturnValue;
import net.minecraft.client.renderer.entity.LivingEntityRenderer;
import net.minecraft.world.entity.Entity;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;

@Mixin(value = LivingEntityRenderer.class, priority = 2000)
public abstract class MixinLivingEntityRenderer {

    @SuppressWarnings("unused")
    @ModifyMethodReturnValue(
            method = "shouldShowName",
            at = @At(value = "INVOKE", target = "Lnet/minecraft/client/Minecraft;getCameraEntity()Lnet/minecraft/world/entity/Entity;"))
    private static Entity onShouldShowNameChangeCameraEntity(Entity entity) {
        return FreeCam.instance.shouldShowMyName() ? null : entity;
    }
}