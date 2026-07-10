use std::{
    path::{self, PathBuf},
    sync::{Arc, Mutex},
    thread, time,
};

use sdl2::{
    controller::{Axis, Button},
    event::{Event, WindowEvent},
    keyboard::Keycode,
    pixels::Color,
    pixels::PixelFormatEnum,
    render::ScaleMode,
};
use tomboyemu_core::{emu::GbEmulator, joypad};
const AXIS_DEAD_ZONE: i16 = 10_000;

fn arc_mutex<T>(inner: T) -> Arc<Mutex<T>> {
    Arc::new(Mutex::new(inner))
}

fn main() {
    let sdl = sdl2::init().unwrap();
    let video = sdl.video().unwrap();
    let mut events = sdl.event_pump().unwrap();
    let controller = sdl.game_controller().unwrap();
    let mut controllers = Vec::new();
    // let timer = sdl.timer().unwrap();

    let window = video
        .window("GbEmu", 256 * 3, 256 * 3)
        .position_centered()
        .resizable()
        .build()
        .unwrap();

    let mut canvas = window
        .into_canvas()
        // .present_vsync()
        .build()
        .unwrap();
    canvas.set_logical_size(256, 256).unwrap();
    let texture_creator = canvas.texture_creator();
    let mut tex = texture_creator
        .create_texture_streaming(PixelFormatEnum::RGBA32, 160, 144)
        .unwrap();
    tex.set_scale_mode(ScaleMode::Nearest);

    println!("Current dir: {:?}", std::env::current_dir());

    let mut bios_path = PathBuf::from("utils/cgb_boot.bin");
    // let mut bios_path = PathBuf::from("utils/dmg_boot.bin");
    let mut rom_path = PathBuf::from("../roms/cgb-acid2.gbc");

    // let emu = GbEmulator::load_bios_only(Some(bios)).unwrap();
    // let emu = GbEmulator::load_rom_from_file(&rom_path, Some(bios)).unwrap();
    let emu = GbEmulator::builder()
        .with_rom_file(&rom_path)
        .with_bios_file(Some(&bios_path))
        .skip_boot(true)
        .build()
        .unwrap();

    let frame_rate = time::Duration::from_secs_f32(1.0 / 60.0);
    let emu = arc_mutex(emu);

    'running: loop {
        // let frame_start = timer.ticks64();
        let frame_start = time::Instant::now();

        for event in events.poll_iter() {
            match event {
                Event::Quit { .. } => break 'running,
                Event::Window { win_event, .. } => match win_event {
                    WindowEvent::Close => break 'running,
                    _ => {}
                },
                Event::DropFile { filename, .. } => {
                    if filename.ends_with(".bin") {
                        bios_path = PathBuf::from(&filename);
                    } else {
                        let new_emu = GbEmulator::builder()
                            .with_rom_file(&filename)
                            .with_bios_file(Some(&bios_path))
                            // .skip_boot(true)
                            .build();

                        match new_emu {
                            Ok(res) => {
                                let mut emu_lock = emu.lock().unwrap();

                                // save current game battery
                                // save_battery(&rom_path, &emu_lock);

                                *emu_lock = res;
                                rom_path = path::PathBuf::from(filename);

                                // load_battery(&rom_path, &mut emu_lock);
                            }
                            Err(e) => eprintln!("{e}"),
                        }
                    }
                }
                Event::KeyDown { keycode, .. } => {
                    if let Some(keycode) = keycode {
                        let mut emu_lock = emu.lock().unwrap();
                        match keycode {
                            Keycode::Up => emu_lock.set_button(joypad::Input::Up, true),
                            Keycode::Left => emu_lock.set_button(joypad::Input::Left, true),
                            Keycode::Down => emu_lock.set_button(joypad::Input::Down, true),
                            Keycode::Right => emu_lock.set_button(joypad::Input::Right, true),
                            Keycode::S => emu_lock.set_button(joypad::Input::A, true),
                            Keycode::A => emu_lock.set_button(joypad::Input::B, true),
                            Keycode::W => emu_lock.set_button(joypad::Input::Start, true),
                            Keycode::E => emu_lock.set_button(joypad::Input::Select, true),
                            #[cfg(feature = "savestates")]
                            Keycode::NUM_9 => emu_lock.savestate("./save.tmp").unwrap(),
                            #[cfg(feature = "savestates")]
                            Keycode::NUM_8 => {
                                emu_lock.loadstate("./save.tmp").unwrap();
                            }
                            Keycode::R => {
                                // save_battery(&rom_path, &emu_lock);
                                // emu_lock.emu_reset();
                                // load_battery(&rom_path, &mut emu_lock);
                            }

                            _ => {}
                        }
                    }
                }

                Event::KeyUp { keycode, .. } => {
                    if let Some(keycode) = keycode {
                        let mut emu_lock = emu.lock().unwrap();
                        match keycode {
                            Keycode::Up => emu_lock.set_button(joypad::Input::Up, false),
                            Keycode::Left => emu_lock.set_button(joypad::Input::Left, false),
                            Keycode::Down => emu_lock.set_button(joypad::Input::Down, false),
                            Keycode::Right => emu_lock.set_button(joypad::Input::Right, false),
                            Keycode::S => emu_lock.set_button(joypad::Input::A, false),
                            Keycode::A => emu_lock.set_button(joypad::Input::B, false),
                            Keycode::W => emu_lock.set_button(joypad::Input::Start, false),
                            Keycode::E => emu_lock.set_button(joypad::Input::Select, false),
                            _ => {}
                        }
                    }
                }

                Event::ControllerButtonDown { button, .. } => {
                    let mut emu_lock = emu.lock().unwrap();
                    match button {
                        Button::DPadUp => emu_lock.set_button(joypad::Input::Up, true),
                        Button::DPadLeft => emu_lock.set_button(joypad::Input::Left, true),
                        Button::DPadDown => emu_lock.set_button(joypad::Input::Down, true),
                        Button::DPadRight => emu_lock.set_button(joypad::Input::Right, true),
                        Button::A => emu_lock.set_button(joypad::Input::A, true),
                        Button::X => emu_lock.set_button(joypad::Input::B, true),
                        Button::Start => emu_lock.set_button(joypad::Input::Start, true),
                        Button::Back => emu_lock.set_button(joypad::Input::Select, true),
                        _ => {}
                    }
                }

                Event::ControllerButtonUp { button, .. } => {
                    let mut emu_lock = emu.lock().unwrap();
                    match button {
                        Button::DPadUp => emu_lock.set_button(joypad::Input::Up, false),
                        Button::DPadLeft => emu_lock.set_button(joypad::Input::Left, false),
                        Button::DPadDown => emu_lock.set_button(joypad::Input::Down, false),
                        Button::DPadRight => emu_lock.set_button(joypad::Input::Right, false),
                        Button::A => emu_lock.set_button(joypad::Input::A, false),
                        Button::X => emu_lock.set_button(joypad::Input::B, false),
                        Button::Start => emu_lock.set_button(joypad::Input::Start, false),
                        Button::Back => emu_lock.set_button(joypad::Input::Select, false),
                        _ => {}
                    }
                }

                Event::ControllerAxisMotion {
                    axis: Axis::LeftX,
                    value,
                    ..
                } => {
                    let mut emu_lock = emu.lock().unwrap();

                    if value > AXIS_DEAD_ZONE {
                        emu_lock.set_button(joypad::Input::Right, true);
                    } else if value < -AXIS_DEAD_ZONE {
                        emu_lock.set_button(joypad::Input::Left, true);
                    } else {
                        emu_lock.set_button(joypad::Input::Left, false);
                        emu_lock.set_button(joypad::Input::Right, false);
                    }
                }
                Event::ControllerAxisMotion {
                    axis: Axis::LeftY,
                    value,
                    ..
                } => {
                    let mut emu_lock = emu.lock().unwrap();

                    if value > AXIS_DEAD_ZONE {
                        emu_lock.set_button(joypad::Input::Down, true);
                    } else if value < -AXIS_DEAD_ZONE {
                        emu_lock.set_button(joypad::Input::Up, true);
                    } else {
                        emu_lock.set_button(joypad::Input::Up, false);
                        emu_lock.set_button(joypad::Input::Down, false);
                    }
                }
                Event::ControllerDeviceAdded { which, .. } => match controller.open(which) {
                    Ok(controller) => {
                        println!("Found controller: {}\n", controller.name());
                        controllers.push(controller);
                    }
                    Err(e) => {
                        eprintln!("A controller was connected, but I couldn't initialize it: {e}\n")
                    }
                },
                _ => {}
            }
        }

        canvas.set_draw_color(Color::GREY);
        canvas.clear();

        {
            let mut emu_lock = emu.lock().unwrap();

            emu_lock.step_until_frame_ready();

            tex.with_lock(None, |pixels, _| {
                pixels.copy_from_slice(emu_lock.get_video_rgba());
                // emu_lock.get_tileset_rgba(pixels);
                // emu_lock.get_tilemap_rgba(pixels);
            })
            .unwrap();
        }

        canvas.copy(&tex, None, None).unwrap();
        canvas.present();

        sleep_until_fps(frame_start, frame_rate);
    }
}

fn sleep_until_fps(frame_start: time::Instant, frame_rate: time::Duration) {
    let frame_duration = frame_start.elapsed();
    if frame_duration < frame_rate {
        thread::sleep(frame_rate - frame_duration);
    }
}
