package com.zergatul.mixin;

import com.zergatul.mixin.helpers.LocalVarHelper;
import org.objectweb.asm.Type;
import org.objectweb.asm.tree.*;
import org.spongepowered.asm.mixin.injection.code.Injector;
import org.spongepowered.asm.mixin.injection.struct.InjectionInfo;
import org.spongepowered.asm.mixin.injection.struct.InjectionNodes;
import org.spongepowered.asm.mixin.injection.struct.Target;
import org.spongepowered.asm.mixin.injection.throwables.InvalidInjectionException;
import org.spongepowered.asm.mixin.injection.throwables.InvalidInjectionPointException;
import org.spongepowered.asm.util.Bytecode;

import static org.objectweb.asm.Opcodes.*;

public class WrapMethodInsideIfConditionInjector extends Injector {

    public WrapMethodInsideIfConditionInjector(InjectionInfo info) {
        super(info, "@WrapMethodInsideIfCondition");
    }

    @Override
    protected void inject(Target target, InjectionNodes.InjectionNode node) {
        checkTargetModifiers(target, false);

        AbstractInsnNode instructionNode = node.getCurrentTarget();
        if (instructionNode instanceof MethodInsnNode methodInsnNode) {
            Type returnType = Type.getReturnType(methodInsnNode.desc);
            if (returnType != Type.VOID_TYPE) {
                throw new InvalidInjectionException(this.info, "@WrapMethodInsideIfCondition should point to void methods.");
            }

            InsnList instructions = new InsnList();
            // TODO: validate parameters
            if (!this.isStatic) {
                throw new InvalidInjectionException(this.info, "@WrapMethodInsideIfCondition - only static supported.");
            }

            Type[] arguments = Type.getArgumentTypes(methodInsnNode.desc);
            if (methodInsnNode.getOpcode() != INVOKESTATIC) {
                // add object instance
                Type[] args2 = new Type[arguments.length + 1];
                args2[0] = Type.getType(Object.class);
                System.arraycopy(arguments, 0, args2, 1, arguments.length);
                arguments = args2;
            }

            int[] argIndexes = new int[arguments.length];
            int startIndex = target.allocateLocals(Bytecode.getArgsSize(arguments));
            for (int i = 0; i < arguments.length; i++) {
                argIndexes[i] = startIndex;
                startIndex += arguments[i].getSize();
            }

            // save stack into variables
            for (int i = arguments.length - 1; i >= 0; i--) {
                instructions.add(new VarInsnNode(LocalVarHelper.getStoreInst(arguments[i]), argIndexes[i]));
            }

            // load from variables into stack
            for (int i = 0; i < arguments.length; i++) {
                instructions.add(new VarInsnNode(LocalVarHelper.getLoadInst(arguments[i]), argIndexes[i]));
            }

            // call mixin method
            invokeHandler(instructions);

            LabelNode label = new LabelNode();
            instructions.add(new JumpInsnNode(IFEQ, label)); // if value is 0 (false), jump

            // load from variables into stack
            for (int i = 0; i < arguments.length; i++) {
                instructions.add(new VarInsnNode(LocalVarHelper.getLoadInst(arguments[i]), argIndexes[i]));
            }

            target.insns.insertBefore(instructionNode, instructions);
            target.insns.insert(instructionNode, label);
        } else {
            throw new InvalidInjectionPointException(this.info, "@WrapMethodInsideIfCondition should point to method instruction.");
        }
    }
}