
cc = meson.get_compiler('c')
m_dep = cc.find_library('m', required : false)
rt_dep = cc.find_library('rt', required : false)
gtk = dependency('gtk4')
assert(gtk.get_pkgconfig_variable('targets').split().contains('wayland'), 'Wayland GDK backend not present')
epoxy = dependency('epoxy')
gmodule_export = dependency('gmodule-export-2.0')

configure_file(input: 'config.h.in', output: 'config.h', configuration: conf)

executable(
  'wdisplays',
  [
    'main.c',
    'outputs.c',
    'render.c',
    'glviewport.c',
    #'overlay.c',
    resources,
  ],
  dependencies : [
    m_dep,
    rt_dep,
    wayland_client,
    client_protos,
    epoxy,
    gtk,
    gmodule_export
  ],
  c_args: ['-DGDK_DISABLE_DEPRECATED', '-DGTK_DISABLE_DEPRECATED'],
  install: true
)
