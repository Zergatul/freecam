package com.zergatul.freecam.mixins;

import com.zergatul.freecam.FreeCamController;
import net.minecraft.client.gui.overlay.DebugOverlayGui;
import org.spongepowered.asm.mixin.Mixin;
import org.spongepowered.asm.mixin.injection.At;
import org.spongepowered.asm.mixin.injection.Inject;
import org.spongepowered.asm.mixin.injection.callback.CallbackInfoReturnable;

import java.util.List;

@Mixin(DebugOverlayGui.class)
public abstract class MixinDebugOverlayGui {

    @Inject(at = @At("TAIL"), method = "Lnet/minecraft/client/gui/overlay/DebugOverlayGui;getGameInformation()Ljava/util/List;")
    private void onGetGameInformation(CallbackInfoReturnable<List<String>> info) {
        FreeCamController.instance.onRenderDebugScreenLeft(info.getReturnValue());
    }

    @Inject(at = @At("TAIL"), method = "Lnet/minecraft/client/gui/overlay/DebugOverlayGui;getSystemInformation()Ljava/util/List;")
    private void onGetSystemInformation(CallbackInfoReturnable<List<String>> info) {
        FreeCamController.instance.onRenderDebugScreenRight(info.getReturnValue());
    }
}