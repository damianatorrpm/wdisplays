
cc = meson.get_compiler('c')
m_dep = cc.find_library('m', required : false)
rt_dep = cc.find_library('rt', required : false)
gdk = dependency('gdk-3.0')
gtk = dependency('gtk+-3.0')
assert(gdk.get_pkgconfig_variable('targets').split().contains('wayland'), 'Wayland GDK backend not present')
epoxy = dependency('epoxy')

wdisplays_src = files(
  'main.c',
  'outputs.c',
  'render.c',
  'glviewport.c',
  'overlay.c'
)
wdisplays_args = []

kanshictl = find_program('kanshictl', required:  get_option('kanshi'))
if kanshictl.found()
  wdisplays_src += files(
    'parser.c'
  )
  wdisplays_args += [
    '-DHAVE_KANSHI',
    '-DKANSHICTL_PATH="@0@"'.format(kanshictl.path())
  ]
endif

executable(
  'wdisplays',
  [
    wdisplays_src,
    resources
  ],
  dependencies : [
    m_dep,
    rt_dep,
    wayland_client,
    client_protos,
    epoxy,
    gtk
  ],
  c_args: wdisplays_args,
  install: true
)
