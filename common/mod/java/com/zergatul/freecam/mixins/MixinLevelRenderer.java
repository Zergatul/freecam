package com.zergatul.freecam.mixins;

import com.mojang.blaze3d.resource.GraphicsResourceAllocator;
import com.zergatul.freecam.FreeCam;
import net.minecraft.client.Camera;
import net.minecraft.client.DeltaTracker;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.client.renderer.LevelRenderer;
import net.minecraft.client.renderer.LightTexture;
import org.joml.Matrix4f;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.ModifyArg;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfo;

@Mixin(LevelRenderer.class)
public abstract class MixinLevelRenderer {

    @ModifyArg(
            method = "renderLevel",
            at = @At(value = "INVOKE", target = "Lnet/minecraft/client/renderer/LevelRenderer;setupRender(Lnet/minecraft/client/Camera;Lnet/minecraft/client/renderer/culling/Frustum;ZZ)V"),
            index = 3)
    private boolean onCallSetupRender(boolean isSpectator) {
        if (FreeCam.instance.isActive()) {
            return true;
        } else {
            return isSpectator;
        }
    }

    @Inject(at = @At("TAIL"), method = "renderLevel")
    private void onRenderLevel(
            GraphicsResourceAllocator allocator,
            DeltaTracker delta,
            boolean renderBlockOutline,
            Camera camera,
            GameRenderer gameRenderer,
            LightTexture lightTexture,
            Matrix4f pose,
            Matrix4f projection,
            CallbackInfo info
    ) {
        FreeCam.instance.onRenderWorldLast(pose, projection, camera);
    }
}