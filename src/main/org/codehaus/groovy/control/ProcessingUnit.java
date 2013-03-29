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

package org.codehaus.groovy.control;

import groovy.lang.GroovyClassLoader;

/**
 * A base class for data structures that can collect messages and errors
 * during processing.
 *
 * @author <a href="mailto:cpoirier@dreaming.org">Chris Poirier</a>
 * @version $Id$
 */

public abstract class ProcessingUnit {

  /**
   * The current phase
   */
  protected int phase;

  /**
   * Set true if phase is finished
   */
  protected boolean phaseComplete;

  /**
   * Configuration and other settings that control processing
   */
  protected CompilerConfiguration configuration;

  /**
   * The ClassLoader to use during processing
   */
  protected GroovyClassLoader classLoader;

  /**
   * A helper to share errors and report them
   */
  protected ErrorCollector errorCollector;


  /**
   * Initialize the {@code ProcessingUnit} to the empty state.
   *
   * @param configuration a {@code CompilerConfiguration}
   * @param classLoader a {@code GroovyClassLoader}
   * @param er an {@code ErrorCollector}.
   */
  public ProcessingUnit(CompilerConfiguration configuration, GroovyClassLoader classLoader, ErrorCollector er) {

    this.phase = Phases.INITIALIZATION;
        this.configuration = configuration;
        this.setClassLoader(classLoader);
        configure((configuration == null ? new CompilerConfiguration() : configuration));
        if (er==null) er = new ErrorCollector(getConfiguration());
        this.errorCollector = er;
    }


  /**
     * Reconfigures the {@code ProcessingUnit}.
     *
     * @param configuration a {@code CompilerConfiguration}
     */
    public void configure(CompilerConfiguration configuration) {
        this.configuration = configuration;
    }


    public CompilerConfiguration getConfiguration() {
        return configuration;
    }

    public void setConfiguration(CompilerConfiguration configuration) {
        this.configuration = configuration;
    }

  /**
     * Returns the class loader in use by this {@code ProcessingUnit}.
     *
     * @return the {@code GroovyClassLoader}
     */
    public GroovyClassLoader getClassLoader() {
        return classLoader;
    }


  /**
     * Sets the class loader for use by this {@code ProcessingUnit}.
     *
     * @param loader a {@code GroovyClassLoader}
     */
    public void setClassLoader(GroovyClassLoader loader) {
        ClassLoader parent = Thread.currentThread().getContextClassLoader();
        if (parent == null) parent = ProcessingUnit.class.getClassLoader();
        this.classLoader = (loader == null ? new GroovyClassLoader(parent, configuration) : loader);
    }


  /**
     * Returns the current phase.
     *
     * @return the current phase.
     */
    public int getPhase() {
        return this.phase;
    }


  /**
     * Returns the description for the current phase.
     *
     * @return description for the current phase.
     */
    public String getPhaseDescription() {
        return Phases.getDescription(this.phase);
    }

  /**
     * Errors found during the compilation should be reported through the {@code ErrorCollector}.
     *
     * @return the {@code ErrorCollector} for this {@code ProcessingUnit}
     */
    public ErrorCollector getErrorCollector() {
        return errorCollector;
    }

    //---------------------------------------------------------------------------
    // PROCESSING


    /**
     * Marks the current phase complete and processes any
     * errors.
     */
    public void completePhase() throws CompilationFailedException {
        errorCollector.failIfErrors();
        phaseComplete = true;
    }


    /**
     * A synonym for {@code gotoPhase( phase + 1 )}.
     */
    public void nextPhase() throws CompilationFailedException {
        gotoPhase(this.phase + 1);
    }


    /**
     * Wraps up any pending operations for the current phase
     * and switches to the next phase.
     *
     * @param phase the phase to go to.
     */
    public void gotoPhase(int phase) throws CompilationFailedException {
        if (!this.phaseComplete) {
            completePhase();
        }

        this.phase = phase;
        this.phaseComplete = false;
    }

}
