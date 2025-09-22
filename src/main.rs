use std::io::{BufReader, Read, Seek};

use gameboy_emulator::{emu::Emu, joypad::{self}};
use sdl2::{event::Event, keyboard::Keycode};

fn load_rom(path: &str) -> Result<Emu, Box<dyn std::error::Error>> {
    let mut bytes = Vec::new();
    let file = std::fs::File::open(path)?;
    let mut reader = BufReader::new(&file);

    _ = zip::read::ZipArchive::new(&mut reader)
        .map_err(|e| e.into())
        .and_then(|mut archive| {
            archive.by_index(0)
            .map_err(|e| e.into())
            .and_then(|mut zip| {
                zip.read_to_end(&mut bytes)
            })
        })
        .or_else(|_| {
            reader.rewind().and_then(|_| reader.read_to_end(&mut bytes))
        })?;

    Emu::new(&bytes).map_err(|e| e.into())
}

fn main() {
    let sdl = sdl2::init().unwrap();
    let video = sdl.video().unwrap();
    let window = video.window("GameboyEmu", 160 * 3, 144 * 3)
        .position_centered()
        .resizable()
        .build().unwrap();
    let mut canvas = window.into_canvas()
        .accelerated()
        .target_texture()
        .build().unwrap();
    canvas.set_logical_size(160, 144).unwrap();

    let texture_creator = canvas.texture_creator();

    let mut tex = texture_creator
        .create_texture_streaming(sdl2::pixels::PixelFormatEnum::RGBA32, 160, 144)
        .unwrap();
    tex.set_scale_mode(sdl2::render::ScaleMode::Nearest);

    let mut events = sdl.event_pump().unwrap();
    let timer = sdl.timer().unwrap();
    let frame_rate = (1.0f64 / 59.73 * 1000.0).round() as u64;

    let mut emu = Emu::new(include_bytes!("../roms/dmg-acid2.gb")).unwrap();

    'running: loop {
        let frame_start = timer.ticks64();

        for event in events.poll_iter() {
            match event {
                Event::Quit {..} => break 'running,
                Event::DropFile { filename, .. } => {
                    let new_emu = load_rom(&filename);
                    match new_emu {
                        Ok(res) => emu = res,
                        Err(e) => eprintln!("{e}"),
                    }
                }
                Event::KeyDown { keycode, .. } => {
                    if let Some(keycode) = keycode {
                        match keycode {
                            Keycode::W => emu.btn_pressed(joypad::UP),
                            Keycode::A => emu.btn_pressed(joypad::LEFT),
                            Keycode::S => emu.btn_pressed(joypad::DOWN),
                            Keycode::D => emu.btn_pressed(joypad::RIGHT),
                            Keycode::K => emu.btn_pressed(joypad::A),
                            Keycode::J => emu.btn_pressed(joypad::B),
                            Keycode::M => emu.btn_pressed(joypad::START),
                            Keycode::N => emu.btn_pressed(joypad::SELECT),
                            _ => {}
                        }
                    }
                }

                Event::KeyUp { keycode, .. } => {
                    if let Some(keycode) = keycode {
                        match keycode {
                            Keycode::W => emu.btn_released(joypad::UP),
                            Keycode::S => emu.btn_released(joypad::DOWN),
                            Keycode::A => emu.btn_released(joypad::LEFT),
                            Keycode::D => emu.btn_released(joypad::RIGHT),
                            Keycode::K => emu.btn_released(joypad::A),
                            Keycode::J => emu.btn_released(joypad::B),
                            Keycode::M => emu.btn_released(joypad::START),
                            Keycode::N => emu.btn_released(joypad::SELECT),
                            _ => {}
                        }
                    }
                }
                _ => {}
            }
        }

        canvas.set_draw_color(sdl2::pixels::Color::GRAY);
        canvas.clear();

        emu.step_until_vblank();

        tex.with_lock(None, |pixels, _| {
            emu.get_framebuf_rgba(pixels);
        }).unwrap();
        canvas.copy(&tex, None, None).unwrap();

        canvas.present();

        let frame_duration = timer.ticks64() - frame_start;
        if frame_duration < frame_rate {
            timer.delay((frame_rate - frame_duration) as u32);
        }
    }
}
