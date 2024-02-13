(function() {var type_impls = {
"object":[["<details class=\"toggle implementors-toggle\" open><summary><section id=\"impl-CoffSymbol%3C'data,+'file,+R,+Coff%3E\" class=\"impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#296-302\">source</a><a href=\"#impl-CoffSymbol%3C'data,+'file,+R,+Coff%3E\" class=\"anchor\">§</a><h3 class=\"code-header\">impl&lt;'data, 'file, R: <a class=\"trait\" href=\"object/read/trait.ReadRef.html\" title=\"trait object::read::ReadRef\">ReadRef</a>&lt;'data&gt;, Coff: <a class=\"trait\" href=\"object/read/coff/trait.CoffHeader.html\" title=\"trait object::read::coff::CoffHeader\">CoffHeader</a>&gt; <a class=\"struct\" href=\"object/read/coff/struct.CoffSymbol.html\" title=\"struct object::read::coff::CoffSymbol\">CoffSymbol</a>&lt;'data, 'file, R, Coff&gt;</h3></section></summary><div class=\"impl-items\"><details class=\"toggle method-toggle\" open><summary><section id=\"method.raw_symbol\" class=\"method\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#299-301\">source</a><h4 class=\"code-header\">pub fn <a href=\"object/read/coff/struct.CoffSymbol.html#tymethod.raw_symbol\" class=\"fn\">raw_symbol</a>(&amp;self) -&gt; &amp;'data Coff::<a class=\"associatedtype\" href=\"object/read/coff/trait.CoffHeader.html#associatedtype.ImageSymbol\" title=\"type object::read::coff::CoffHeader::ImageSymbol\">ImageSymbol</a></h4></section></summary><div class=\"docblock\"><p>Get the raw <code>ImageSymbol</code> struct.</p>\n</div></details></div></details>",0,"object::read::coff::symbol::CoffBigSymbol"],["<details class=\"toggle implementors-toggle\" open><summary><section id=\"impl-Clone-for-CoffSymbol%3C'data,+'file,+R,+Coff%3E\" class=\"impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#285\">source</a><a href=\"#impl-Clone-for-CoffSymbol%3C'data,+'file,+R,+Coff%3E\" class=\"anchor\">§</a><h3 class=\"code-header\">impl&lt;'data, 'file, R, Coff&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/clone/trait.Clone.html\" title=\"trait core::clone::Clone\">Clone</a> for <a class=\"struct\" href=\"object/read/coff/struct.CoffSymbol.html\" title=\"struct object::read::coff::CoffSymbol\">CoffSymbol</a>&lt;'data, 'file, R, Coff&gt;<div class=\"where\">where\n    R: <a class=\"trait\" href=\"object/read/trait.ReadRef.html\" title=\"trait object::read::ReadRef\">ReadRef</a>&lt;'data&gt; + <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/clone/trait.Clone.html\" title=\"trait core::clone::Clone\">Clone</a>,\n    Coff: <a class=\"trait\" href=\"object/read/coff/trait.CoffHeader.html\" title=\"trait object::read::coff::CoffHeader\">CoffHeader</a> + <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/clone/trait.Clone.html\" title=\"trait core::clone::Clone\">Clone</a>,\n    Coff::<a class=\"associatedtype\" href=\"object/read/coff/trait.CoffHeader.html#associatedtype.ImageSymbol\" title=\"type object::read::coff::CoffHeader::ImageSymbol\">ImageSymbol</a>: <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/clone/trait.Clone.html\" title=\"trait core::clone::Clone\">Clone</a>,</div></h3></section></summary><div class=\"impl-items\"><details class=\"toggle method-toggle\" open><summary><section id=\"method.clone\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#285\">source</a><a href=\"#method.clone\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"https://doc.rust-lang.org/1.76.0/core/clone/trait.Clone.html#tymethod.clone\" class=\"fn\">clone</a>(&amp;self) -&gt; <a class=\"struct\" href=\"object/read/coff/struct.CoffSymbol.html\" title=\"struct object::read::coff::CoffSymbol\">CoffSymbol</a>&lt;'data, 'file, R, Coff&gt;</h4></section></summary><div class='docblock'>Returns a copy of the value. <a href=\"https://doc.rust-lang.org/1.76.0/core/clone/trait.Clone.html#tymethod.clone\">Read more</a></div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.clone_from\" class=\"method trait-impl\"><span class=\"rightside\"><span class=\"since\" title=\"Stable since Rust version 1.0.0\">1.0.0</span> · <a class=\"src\" href=\"https://doc.rust-lang.org/1.76.0/src/core/clone.rs.html#169\">source</a></span><a href=\"#method.clone_from\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"https://doc.rust-lang.org/1.76.0/core/clone/trait.Clone.html#method.clone_from\" class=\"fn\">clone_from</a>(&amp;mut self, source: <a class=\"primitive\" href=\"https://doc.rust-lang.org/1.76.0/core/primitive.reference.html\">&amp;Self</a>)</h4></section></summary><div class='docblock'>Performs copy-assignment from <code>source</code>. <a href=\"https://doc.rust-lang.org/1.76.0/core/clone/trait.Clone.html#method.clone_from\">Read more</a></div></details></div></details>","Clone","object::read::coff::symbol::CoffBigSymbol"],["<section id=\"impl-Copy-for-CoffSymbol%3C'data,+'file,+R,+Coff%3E\" class=\"impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#285\">source</a><a href=\"#impl-Copy-for-CoffSymbol%3C'data,+'file,+R,+Coff%3E\" class=\"anchor\">§</a><h3 class=\"code-header\">impl&lt;'data, 'file, R, Coff&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/marker/trait.Copy.html\" title=\"trait core::marker::Copy\">Copy</a> for <a class=\"struct\" href=\"object/read/coff/struct.CoffSymbol.html\" title=\"struct object::read::coff::CoffSymbol\">CoffSymbol</a>&lt;'data, 'file, R, Coff&gt;<div class=\"where\">where\n    R: <a class=\"trait\" href=\"object/read/trait.ReadRef.html\" title=\"trait object::read::ReadRef\">ReadRef</a>&lt;'data&gt; + <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/marker/trait.Copy.html\" title=\"trait core::marker::Copy\">Copy</a>,\n    Coff: <a class=\"trait\" href=\"object/read/coff/trait.CoffHeader.html\" title=\"trait object::read::coff::CoffHeader\">CoffHeader</a> + <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/marker/trait.Copy.html\" title=\"trait core::marker::Copy\">Copy</a>,\n    Coff::<a class=\"associatedtype\" href=\"object/read/coff/trait.CoffHeader.html#associatedtype.ImageSymbol\" title=\"type object::read::coff::CoffHeader::ImageSymbol\">ImageSymbol</a>: <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/marker/trait.Copy.html\" title=\"trait core::marker::Copy\">Copy</a>,</div></h3></section>","Copy","object::read::coff::symbol::CoffBigSymbol"],["<details class=\"toggle implementors-toggle\" open><summary><section id=\"impl-Debug-for-CoffSymbol%3C'data,+'file,+R,+Coff%3E\" class=\"impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#285\">source</a><a href=\"#impl-Debug-for-CoffSymbol%3C'data,+'file,+R,+Coff%3E\" class=\"anchor\">§</a><h3 class=\"code-header\">impl&lt;'data, 'file, R, Coff&gt; <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/fmt/trait.Debug.html\" title=\"trait core::fmt::Debug\">Debug</a> for <a class=\"struct\" href=\"object/read/coff/struct.CoffSymbol.html\" title=\"struct object::read::coff::CoffSymbol\">CoffSymbol</a>&lt;'data, 'file, R, Coff&gt;<div class=\"where\">where\n    R: <a class=\"trait\" href=\"object/read/trait.ReadRef.html\" title=\"trait object::read::ReadRef\">ReadRef</a>&lt;'data&gt; + <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/fmt/trait.Debug.html\" title=\"trait core::fmt::Debug\">Debug</a>,\n    Coff: <a class=\"trait\" href=\"object/read/coff/trait.CoffHeader.html\" title=\"trait object::read::coff::CoffHeader\">CoffHeader</a> + <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/fmt/trait.Debug.html\" title=\"trait core::fmt::Debug\">Debug</a>,\n    Coff::<a class=\"associatedtype\" href=\"object/read/coff/trait.CoffHeader.html#associatedtype.ImageSymbol\" title=\"type object::read::coff::CoffHeader::ImageSymbol\">ImageSymbol</a>: <a class=\"trait\" href=\"https://doc.rust-lang.org/1.76.0/core/fmt/trait.Debug.html\" title=\"trait core::fmt::Debug\">Debug</a>,</div></h3></section></summary><div class=\"impl-items\"><details class=\"toggle method-toggle\" open><summary><section id=\"method.fmt\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#285\">source</a><a href=\"#method.fmt\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"https://doc.rust-lang.org/1.76.0/core/fmt/trait.Debug.html#tymethod.fmt\" class=\"fn\">fmt</a>(&amp;self, f: &amp;mut <a class=\"struct\" href=\"https://doc.rust-lang.org/1.76.0/core/fmt/struct.Formatter.html\" title=\"struct core::fmt::Formatter\">Formatter</a>&lt;'_&gt;) -&gt; <a class=\"type\" href=\"https://doc.rust-lang.org/1.76.0/core/fmt/type.Result.html\" title=\"type core::fmt::Result\">Result</a></h4></section></summary><div class='docblock'>Formats the value using the given formatter. <a href=\"https://doc.rust-lang.org/1.76.0/core/fmt/trait.Debug.html#tymethod.fmt\">Read more</a></div></details></div></details>","Debug","object::read::coff::symbol::CoffBigSymbol"],["<details class=\"toggle implementors-toggle\" open><summary><section id=\"impl-ObjectSymbol%3C'data%3E-for-CoffSymbol%3C'data,+'file,+R,+Coff%3E\" class=\"impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#309-506\">source</a><a href=\"#impl-ObjectSymbol%3C'data%3E-for-CoffSymbol%3C'data,+'file,+R,+Coff%3E\" class=\"anchor\">§</a><h3 class=\"code-header\">impl&lt;'data, 'file, R: <a class=\"trait\" href=\"object/read/trait.ReadRef.html\" title=\"trait object::read::ReadRef\">ReadRef</a>&lt;'data&gt;, Coff: <a class=\"trait\" href=\"object/read/coff/trait.CoffHeader.html\" title=\"trait object::read::coff::CoffHeader\">CoffHeader</a>&gt; <a class=\"trait\" href=\"object/read/trait.ObjectSymbol.html\" title=\"trait object::read::ObjectSymbol\">ObjectSymbol</a>&lt;'data&gt; for <a class=\"struct\" href=\"object/read/coff/struct.CoffSymbol.html\" title=\"struct object::read::coff::CoffSymbol\">CoffSymbol</a>&lt;'data, 'file, R, Coff&gt;</h3></section></summary><div class=\"impl-items\"><details class=\"toggle method-toggle\" open><summary><section id=\"method.index\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#313-315\">source</a><a href=\"#method.index\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#tymethod.index\" class=\"fn\">index</a>(&amp;self) -&gt; <a class=\"struct\" href=\"object/read/struct.SymbolIndex.html\" title=\"struct object::read::SymbolIndex\">SymbolIndex</a></h4></section></summary><div class='docblock'>The index of the symbol.</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.name_bytes\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#317-325\">source</a><a href=\"#method.name_bytes\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#tymethod.name_bytes\" class=\"fn\">name_bytes</a>(&amp;self) -&gt; <a class=\"type\" href=\"object/read/type.Result.html\" title=\"type object::read::Result\">Result</a>&lt;&amp;'data [<a class=\"primitive\" href=\"https://doc.rust-lang.org/1.76.0/core/primitive.u8.html\">u8</a>]&gt;</h4></section></summary><div class='docblock'>The name of the symbol.</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.name\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#327-332\">source</a><a href=\"#method.name\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#tymethod.name\" class=\"fn\">name</a>(&amp;self) -&gt; <a class=\"type\" href=\"object/read/type.Result.html\" title=\"type object::read::Result\">Result</a>&lt;&amp;'data <a class=\"primitive\" href=\"https://doc.rust-lang.org/1.76.0/core/primitive.str.html\">str</a>&gt;</h4></section></summary><div class='docblock'>The name of the symbol. <a href=\"object/read/trait.ObjectSymbol.html#tymethod.name\">Read more</a></div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.address\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#334-351\">source</a><a href=\"#method.address\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#tymethod.address\" class=\"fn\">address</a>(&amp;self) -&gt; <a class=\"primitive\" href=\"https://doc.rust-lang.org/1.76.0/core/primitive.u64.html\">u64</a></h4></section></summary><div class='docblock'>The address of the symbol. May be zero if the address is unknown.</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.size\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#353-386\">source</a><a href=\"#method.size\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#tymethod.size\" class=\"fn\">size</a>(&amp;self) -&gt; <a class=\"primitive\" href=\"https://doc.rust-lang.org/1.76.0/core/primitive.u64.html\">u64</a></h4></section></summary><div class='docblock'>The size of the symbol. May be zero if the size is unknown.</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.kind\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#388-408\">source</a><a href=\"#method.kind\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#tymethod.kind\" class=\"fn\">kind</a>(&amp;self) -&gt; <a class=\"enum\" href=\"object/enum.SymbolKind.html\" title=\"enum object::SymbolKind\">SymbolKind</a></h4></section></summary><div class='docblock'>Return the kind of this symbol.</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.section\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#410-436\">source</a><a href=\"#method.section\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#tymethod.section\" class=\"fn\">section</a>(&amp;self) -&gt; <a class=\"enum\" href=\"object/read/enum.SymbolSection.html\" title=\"enum object::read::SymbolSection\">SymbolSection</a></h4></section></summary><div class='docblock'>Returns the section where the symbol is defined.</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.is_undefined\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#439-443\">source</a><a href=\"#method.is_undefined\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#tymethod.is_undefined\" class=\"fn\">is_undefined</a>(&amp;self) -&gt; <a class=\"primitive\" href=\"https://doc.rust-lang.org/1.76.0/core/primitive.bool.html\">bool</a></h4></section></summary><div class='docblock'>Return true if the symbol is undefined.</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.is_definition\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#446-448\">source</a><a href=\"#method.is_definition\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#tymethod.is_definition\" class=\"fn\">is_definition</a>(&amp;self) -&gt; <a class=\"primitive\" href=\"https://doc.rust-lang.org/1.76.0/core/primitive.bool.html\">bool</a></h4></section></summary><div class='docblock'>Return true if the symbol is a definition of a function or data object\nthat has a known address. <a href=\"object/read/trait.ObjectSymbol.html#tymethod.is_definition\">Read more</a></div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.is_common\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#451-455\">source</a><a href=\"#method.is_common\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#tymethod.is_common\" class=\"fn\">is_common</a>(&amp;self) -&gt; <a class=\"primitive\" href=\"https://doc.rust-lang.org/1.76.0/core/primitive.bool.html\">bool</a></h4></section></summary><div class='docblock'>Return true if the symbol is common data. <a href=\"object/read/trait.ObjectSymbol.html#tymethod.is_common\">Read more</a></div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.is_weak\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#458-460\">source</a><a href=\"#method.is_weak\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#tymethod.is_weak\" class=\"fn\">is_weak</a>(&amp;self) -&gt; <a class=\"primitive\" href=\"https://doc.rust-lang.org/1.76.0/core/primitive.bool.html\">bool</a></h4></section></summary><div class='docblock'>Return true if the symbol is weak.</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.scope\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#463-471\">source</a><a href=\"#method.scope\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#tymethod.scope\" class=\"fn\">scope</a>(&amp;self) -&gt; <a class=\"enum\" href=\"object/enum.SymbolScope.html\" title=\"enum object::SymbolScope\">SymbolScope</a></h4></section></summary><div class='docblock'>Returns the symbol scope.</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.is_global\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#474-479\">source</a><a href=\"#method.is_global\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#tymethod.is_global\" class=\"fn\">is_global</a>(&amp;self) -&gt; <a class=\"primitive\" href=\"https://doc.rust-lang.org/1.76.0/core/primitive.bool.html\">bool</a></h4></section></summary><div class='docblock'>Return true if the symbol visible outside of the compilation unit. <a href=\"object/read/trait.ObjectSymbol.html#tymethod.is_global\">Read more</a></div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.is_local\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#482-484\">source</a><a href=\"#method.is_local\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#tymethod.is_local\" class=\"fn\">is_local</a>(&amp;self) -&gt; <a class=\"primitive\" href=\"https://doc.rust-lang.org/1.76.0/core/primitive.bool.html\">bool</a></h4></section></summary><div class='docblock'>Return true if the symbol is only visible within the compilation unit.</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.flags\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/coff/symbol.rs.html#486-505\">source</a><a href=\"#method.flags\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#tymethod.flags\" class=\"fn\">flags</a>(&amp;self) -&gt; <a class=\"enum\" href=\"object/enum.SymbolFlags.html\" title=\"enum object::SymbolFlags\">SymbolFlags</a>&lt;<a class=\"struct\" href=\"object/read/struct.SectionIndex.html\" title=\"struct object::read::SectionIndex\">SectionIndex</a>, <a class=\"struct\" href=\"object/read/struct.SymbolIndex.html\" title=\"struct object::read::SymbolIndex\">SymbolIndex</a>&gt;</h4></section></summary><div class='docblock'>Symbol flags that are specific to each file format.</div></details><details class=\"toggle method-toggle\" open><summary><section id=\"method.section_index\" class=\"method trait-impl\"><a class=\"src rightside\" href=\"src/object/read/traits.rs.html#504-506\">source</a><a href=\"#method.section_index\" class=\"anchor\">§</a><h4 class=\"code-header\">fn <a href=\"object/read/trait.ObjectSymbol.html#method.section_index\" class=\"fn\">section_index</a>(&amp;self) -&gt; <a class=\"enum\" href=\"https://doc.rust-lang.org/1.76.0/core/option/enum.Option.html\" title=\"enum core::option::Option\">Option</a>&lt;<a class=\"struct\" href=\"object/read/struct.SectionIndex.html\" title=\"struct object::read::SectionIndex\">SectionIndex</a>&gt;</h4></section></summary><div class='docblock'>Returns the section index for the section containing this symbol. <a href=\"object/read/trait.ObjectSymbol.html#method.section_index\">Read more</a></div></details></div></details>","ObjectSymbol<'data>","object::read::coff::symbol::CoffBigSymbol"]]
};if (window.register_type_impls) {window.register_type_impls(type_impls);} else {window.pending_type_impls = type_impls;}})()