import React, {useState} from 'react';
import Link from '@docusaurus/Link';
import Heading from '@theme/Heading';
import Layout from '@theme/Layout';
import useBaseUrl from '@docusaurus/useBaseUrl';

import styles from './index.module.css';

type DemoKey = 'script' | 'compile' | 'dotnet' | 'javascript';

const demos: Record<DemoKey, {label: string; filename: string; language: string; code: string}> = {
  script: {
    label: 'The script',
    filename: 'pricing.fss',
    language: 'FScript',
    code: `type Customer =
  { Name: string
    Plan: string
    Seats: int }

export let quote (customer: Customer) =
  let basePrice =
    match customer.Plan with
    | "scale" -> 29
    | "team" -> 19
    | _ -> 9

  basePrice * customer.Seats`,
  },
  compile: {
    label: 'Compile',
    filename: 'terminal',
    language: 'Shell',
    code: `# Iterate with the interpreter
fscript pricing.fss

# Build an optimized .NET assembly
fscript compile pricing.fss --target dotnet \\
  --output out/pricing
dotnet out/pricing/pricing.dll

# Publish a native executable for this platform
fscript compile pricing.fss --target dotnet \\
  --aot --output out/pricing-native

# Or generate a buildable C# project
fscript compile pricing.fss --target csharp \\
  --output out/pricing-csharp`,
  },
  dotnet: {
    label: '.NET host',
    filename: 'Pricing.fs',
    language: 'F#',
    code: `open System.IO
open FScript.Language
open FScript.Runtime

let source = File.ReadAllText "pricing.fss"
let externs = Registry.all hostContext
let pricing = ScriptHost.loadSource externs source

let customer =
  VRecord (Map.ofList
    [ "Name", VString "Acme"
      "Plan", VString "scale"
      "Seats", VInt 12L ])

let total =
  ScriptHost.invoke pricing "quote" [ customer ]
// Change the rule. Keep the host.`,
  },
  javascript: {
    label: 'JavaScript host',
    filename: 'pricing.mjs',
    language: 'JavaScript',
    code: `import { load, invoke } from
  "@magnusopera/fscript";

const pricing = load(source);

const total = invoke(pricing, "quote", [{
  Name: "Acme",
  Plan: "scale",
  Seats: 12
}]);

// The language runs inside your process.`,
  },
};

function Arrow() {
  return <span aria-hidden="true">↗</span>;
}

function DemoWindow() {
  const [activeDemo, setActiveDemo] = useState<DemoKey>('script');
  const demo = demos[activeDemo];

  return (
    <div className={styles.demoWindow}>
      <div className={styles.windowBar}>
        <div className={styles.windowDots} aria-hidden="true"><i /><i /><i /></div>
        <span className={styles.windowTitle}>{demo.filename}</span>
        <span className={styles.liveBadge}><i /> {activeDemo === 'compile' ? 'standalone' : 'in-process'}</span>
      </div>
      <div className={styles.demoTabs} role="tablist" aria-label="Interpret, compile, or embed FScript">
        {(Object.keys(demos) as DemoKey[]).map((key) => (
          <button
            key={key}
            type="button"
            role="tab"
            aria-selected={activeDemo === key}
            className={activeDemo === key ? styles.demoTabActive : styles.demoTab}
            onClick={() => setActiveDemo(key)}>
            {demos[key].label}
          </button>
        ))}
      </div>
      <div className={styles.codeArea}>
        <div className={styles.lineNumbers} aria-hidden="true">
          {demo.code.split('\n').map((_, index) => <span key={index}>{index + 1}</span>)}
        </div>
        <pre><code>{demo.code}</code></pre>
      </div>
      <div className={styles.windowFooter}>
        <span><i className={styles.statusDot} /> type checked</span>
        <span>{demo.language}</span>
      </div>
    </div>
  );
}

function HomepageHeader() {
  const mascot = useBaseUrl('/img/logo.svg');

  return (
    <header className={styles.hero}>
      <div className={styles.heroGrid} aria-hidden="true" />
      <div className={styles.orbOne} aria-hidden="true" />
      <div className={styles.orbTwo} aria-hidden="true" />
      <div className={`container ${styles.heroInner}`}>
        <div className={styles.heroCopy}>
          <div className={styles.eyebrow}><span>Open source</span> · Script · Embed · Compile</div>
          <Heading as="h1" className={styles.heroTitle}>
            Script. Embed.<br /><em>Compile.</em>
          </Heading>
          <p className={styles.heroLead}>
            FScript is a typed, functional scripting language for standalone programs and application scripting.
            Run scripts with the interpreter, embed them in .NET and JavaScript applications, or compile
            them to optimized .NET and native executables.
          </p>
          <div className={styles.heroActions}>
            <Link className={styles.primaryAction} to="/sandbox">
              Try FScript <Arrow />
            </Link>
            <Link className={styles.secondaryAction} to="#run-scripts">
              Explore the options
            </Link>
          </div>
          <div className={styles.installLine}>
            <span className={styles.prompt}>$</span>
            <code>dotnet tool install --global MagnusOpera.FScript</code>
          </div>
          <div className={styles.heroProof}>
            <span>Static type inference</span>
            <span>.NET &amp; JavaScript embedding</span>
            <span>Typed host APIs</span>
            <span>Optimized compilation</span>
          </div>
        </div>
        <div className={styles.heroVisual}>
          <div className={styles.mascotMark}>
            <img src={mascot} alt="" width="92" height="92" />
            <span>YOUR CODE</span>
          </div>
          <DemoWindow />
        </div>
      </div>
      <div className={styles.heroTicker} aria-label="How FScript runs">
        <div>
          <span>RUN SCRIPTS</span><i>✦</i><span>EMBED IN .NET</span><i>✦</i><span>EMBED IN JAVASCRIPT</span><i>✦</i>
          <span>COMPILE .NET</span><i>✦</i><span>PUBLISH NATIVE AOT</span><i>✦</i>
        </div>
      </div>
    </header>
  );
}

export default function Home() {
  return (
    <Layout
      title="FScript: Script. Embed. Compile."
      description="FScript is a typed, functional scripting language for standalone programs and application scripting. Run scripts, embed in .NET and JavaScript, or compile to .NET and native executables.">
      <HomepageHeader />
      <main className={styles.main}>
        <section className={styles.workflowScene} aria-label="Three ways to use FScript">
          <div className={`container ${styles.workflowGrid}`}>
            <div>
              <p className={styles.sectionLabel}>Script</p>
              <Heading as="h2" id="run-scripts">Run scripts.</Heading>
              <p>
                Write standalone programs, automate everyday tasks, and explore ideas in the REPL.
                Run <code>fscript program.fss</code> for a simple edit-and-run workflow, with static
                type inference checking your code before execution.
              </p>
              <p>
                Use immutable data, pattern matching, and the standard library to express your logic.
                Connect to .NET libraries through typed FFI when your program needs more.
              </p>
              <Link className={styles.inlineLink} to="/manual/learn/quickstart">Install and run <Arrow /></Link>
              <Link className={styles.inlineLink} to="/sandbox">Try the playground <Arrow /></Link>
            </div>
            <div>
              <p className={styles.sectionLabel}>Embed</p>
              <Heading as="h2">Script your app.</Heading>
              <p>
                Embed FScript in .NET and JavaScript applications to make pricing, eligibility,
                workflows, or customer-specific rules configurable. Load scripts, inspect exported
                function types, and invoke their exports from your application.
              </p>
              <p>
                The host chooses which typed functions and resources scripts can use and where
                imported source comes from. Load updated rules without rebuilding the application.
              </p>
              <Link className={styles.inlineLink} to="/manual/embedding/overview">Embed FScript <Arrow /></Link>
              <Link className={styles.inlineLink} to="/manual/embedding/register-externs">Expose typed host APIs <Arrow /></Link>
            </div>
            <div>
              <p className={styles.sectionLabel}>Compile</p>
              <Heading as="h2">Build programs.</Heading>
              <p>
                Compile standalone programs to optimized .NET assemblies, publish self-contained
                executables, or use Native AOT to produce native binaries. Generated programs run
                without an FScript interpreter.
              </p>
              <p>
                The compiler specializes functions, turns self tail calls into loops, and fuses eligible
                pure collection pipelines. Use typed .NET FFI bindings, or generate a C# project
                to integrate into your own build.
              </p>
              <Link className={styles.inlineLink} to="/manual/compiler/overview">Compile a program <Arrow /></Link>
              <Link className={styles.inlineLink} to="/manual/compiler/optimizations">Explore the optimizations <Arrow /></Link>
            </div>
          </div>
        </section>

        <section className={styles.languageScene}>
          <div className={`container ${styles.sceneIntro}`}>
            <p className={styles.sectionLabel}>Language lineage</p>
            <Heading as="h2">Name the thing.<br /><em>Or just ask for its shape.</em></Heading>
            <div className={styles.sceneSummary}>
              <p className={styles.sceneLead}>
                FScript keeps the part of the ML family that makes programs pleasant to reason about: inferred
                types, immutable data, unions, curried functions, and pattern matching. Then it adds a useful choice.
                A record can carry a domain name, or a function can accept any value with the fields it needs.
              </p>
              <Link className={styles.inlineLink} to="/manual/learn/language-tour">Meet the language <Arrow /></Link>
            </div>
          </div>
        </section>

        <section className={styles.javascriptScene}>
          <div className={`container ${styles.javascriptGrid}`}>
            <div className={styles.browserStage} aria-label="FScript running in a browser">
              <div className={styles.browserChrome}><i /><i /><i /><span>app.example / live-pricing</span></div>
              <div className={styles.browserBody}>
                <div className={styles.browserEditor}>
                  <span>rules/discount.fss · loaded</span>
                  <pre><code>{`let discount customer =
  match customer.Plan with
  | "scale" -> 20
  | "team"  -> 10
  | _       -> 0`}</code></pre>
                </div>
                <div className={styles.browserResult}>
                  <span>TYPE</span><code>Customer → int</code>
                  <span>RESULT</span><strong>20</strong>
                </div>
              </div>
              <div className={styles.browserStatus}><span><i /> checked in this tab</span><span>npm · ESM</span></div>
            </div>
            <div className={styles.javascriptCopy}>
              <p className={styles.sectionLabel}>JavaScript support</p>
              <Heading as="h2">Ship the app. Change the rules while it runs.</Heading>
              <p>
                FScript is an npm package for browsers and Node. Keep pricing, eligibility, feature decisions, or
                customer-specific policy outside the application bundle. Fetch a rule when you need it, check its
                types, and call its exported functions from JavaScript.
              </p>
              <p>
                The host supplies typed operations and decides where rule files come from. A new rule can take effect
                without rebuilding the interface or restarting the process. The application keeps running; the part
                you meant to make configurable is free to change.
              </p>
              <Link className={styles.inlineLink} to="/manual/embedding/fable-javascript">Load rules from JavaScript <Arrow /></Link>
            </div>
          </div>
        </section>

        <section className={styles.finalLinks}>
          <div className={`container ${styles.finalLinksInner}`}>
            <p>FScript is open source.</p>
            <nav aria-label="Start using FScript">
              <Link to="/sandbox">Open the sandbox</Link>
              <Link to="/manual/learn/quickstart">Install and run</Link>
              <Link to="/manual/embedding/overview">Embed FScript</Link>
              <Link to="/manual/compiler/overview">Compile a program</Link>
              <a href="https://github.com/MagnusOpera/FScript">GitHub <Arrow /></a>
            </nav>
          </div>
        </section>
      </main>
    </Layout>
  );
}
