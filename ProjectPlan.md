# Project Plan: Multilevel Parallel Patterns in Erlang -> Stream Parallel Patterns for Distributed Batch Computing

## 1. Executive Summary

This project aims to address the gap in distributed computing by developing pattern libraries for stream parallel patterns specifically designed for distributed batch computing environments. The focus is on adapting farm and pipeline patterns for efficient execution, enhancing computational efficiency for embarrassingly parallel problems, and integrating with distributed processing engines like Apache Spark and Apache Flink. The project utilizes Erlang's actor model and functional programming paradigm to create a Farm Parallel Pattern Library and a Pipeline Parallel Pattern Library.

## 2. Project Objectives

- **Adaptation for Distributed Systems**: Modify and tailor parallel patterns (farm and pipeline) for distributed computing environments.

- **Computational Efficiency Testing**: Assess the adapted patterns' effectiveness in handling batch workloads and evaluate performance improvements.

- **Development of Engine Bindings**: Create bindings for Apache Spark and Apache Flink to demonstrate practical applicability.

## 3. Project Phases

### 3.1 Research and Planning (Weeks 1-3, May)

**Objective**: Establish a solid foundation for the project.

- **Task 1.1**: Conduct literature review on parallel patterns and distributed computing.

- **Task 1.2**: Define project scope and initial requirements.

- **Deliverable**: Research report and initial project requirements document.

### 3.2 Design (Weeks 4-6, June)

**Objective**: Develop architectural design and specifications for the pattern libraries.

- **Task 2.1**: Design architecture for the pattern library.

- **Task 2.2**: Specify farm and pipeline patterns for distributed systems.

- **Deliverable**: Design document and specifications for farm and pipeline patterns.

### 3.3 Implementation (Weeks 7-9, June-July)

**Objective**: Develop core functionalities and initial versions of patterns.

- **Task 3.1**: Develop core functionalities of the parallel pattern library.

- **Task 3.2**: Implement basic versions of farm and pipeline patterns.

- **Deliverable**: Prototype pattern libraries with basic implementations.

### 3.4 Agile Iterations (Weeks 10-13, July-August)

**Objective**: Refine, optimize, and integrate the pattern libraries.

**Sprint 1**:

- **Task 4.1**: Refine and optimize parallel pattern libraries.

- **Deliverable**: Optimized pattern libraries.

**Sprint 2**:

- **Task 4.2**: Conduct performance testing and benchmarking.

- **Deliverable**: Performance evaluation report.

**Sprint 3**:

- **Task 4.3**: Develop engine bindings for Apache Spark and Apache Flink.

- **Deliverable**: Engine bindings and integration documentation.

### 3.5 Documentation and Final Testing (Weeks 14-15, August)

**Objective**: Finalize testing and project documentation.

- **Task 5.1**: Perform comprehensive testing of the stream parallel pattern libraries.

- **Task 5.2**: Prepare and finalize project documentation.

- **Deliverable**: Final test report and completed project documentation.

## 4. Development Approach

A hybrid Waterfall-Agile approach was utilized:

- **Waterfall**: Initial phases (Research, Planning, and Design) to establish a solid foundation.

- **Agile**: Later stages (Implementation and Agile Iterations) for flexibility and rapid iteration.

## 5. Project Timeline

| Phase | Tasks | Duration | Start Date | End Date |

|-------|-------|----------|------------|----------|

| Research and Planning | Literature review, define scope and requirements | 3 weeks | May 1, 2024 | May 21, 2024 |

| Design | Architecture design, pattern specification | 3 weeks | May 22, 2024 | June 11, 2024 |

| Implementation | Develop core functionalities, implement patterns | 3 weeks | June 12, 2024 | July 2, 2024 |

| Agile Iterations | Refine patterns, performance testing, engine bindings | 4 weeks | July 3, 2024 | August 4, 2024 |

| Documentation and Final Testing | Comprehensive testing, finalize documentation | 2 weeks | August 5, 2024 | August 19, 2024 |

| Project Review and Submission | Final review, project submission | 1 week | August 20, 2024 | August 26, 2024 |

## 6. Roles and Responsibilities

- **Project Lead**: [Your Name]

  - Oversee project execution, manage development phases, and ensure adherence to timelines.

- **Supervisor**: Dr. Vladimir Janjic

  - Provide guidance, feedback, and oversight, particularly during Agile sprints.

- **Development Team**: [If applicable]

  - Assist in coding, testing, and documentation tasks.

## 7. Tools and Artifacts

- **Project Management Tools**: To-do list, git for version control.

- **Documentation Tools**: Word processors, design tools.

- **Testing Tools**: Performance benchmarking tools, containerization software.

## 8. Risk Management

- **Potential Risks**: Delays in implementation, integration challenges, performance issues.

- **Mitigation Strategies**: Regular check-ins, Agile flexibility, early testing.

## 9. Evaluation and Review

- **Progress Tracking**: Weekly meetings, sprint reviews, and daily scrums.

- **Performance Metrics**: Benchmarking results, code efficiency, and pattern library effectiveness.