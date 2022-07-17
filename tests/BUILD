python_sources()

resources(
    name="fixtures",
    sources=[
        "fixtures/*",
    ]
)

resources(
    name="support",
    sources=[
        "support/*",
    ]
)

python_tests(
    name="tests0",
    dependencies=[
        "//:lib",
        "sphinxcontrib/cldomain:lisp_lib",
        ":fixtures",
        ":support",
    ],
)
