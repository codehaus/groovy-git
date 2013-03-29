/*
 * Copyright 2003-2007, 2013 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package org.codehaus.groovy.ast;

/**
 * A special visitor for working with the structure of a class. In general, your
 * will want to use the Abstract class based on this class {@link ClassCodeVisitorSupport}.
 *
 * @see org.codehaus.groovy.ast.ClassNode
 * @see org.codehaus.groovy.ast.ClassCodeVisitorSupport
 *
 * @author <a href="mailto:james@coredevelopers.net">James Strachan</a>
 * @version $Revision$
 */
public interface GroovyClassVisitor {

  /**
    * Visit a ClassNode.
    *
    * @param node the node to visit.
    */
    void visitClass(ClassNode node);

  /**
    * Visit a ConstructorNode.
    *
    * @param node the node to visit.
    */
    void visitConstructor(ConstructorNode node);

  /**
    * Visit a MethodNode.
    *
    * @param node the node to visit.
    */
    void visitMethod(MethodNode node);

  /**
    * Visit a FieldNode.
    *
    * @param node the node to visit.
    */
    void visitField(FieldNode node);

  /**
    * Visit a PropertyNode.
    *
    * @param node the node to visit.
    */
    void visitProperty(PropertyNode node);
}
