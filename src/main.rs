use gameboy_emulator::{emu::Emu, joypad::{self, *}};
use sdl2::{event::Event, keyboard::Keycode};

fn main() {
    let sdl = sdl2::init().unwrap();
    let video = sdl.video().unwrap();
    let window = video.window("GameboyEmu", 160 * 3, 144 * 3)
        .position_centered()
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

    let mut emu = Emu::from_slice(include_bytes!("../roms/Tennis (JUE) [!].gb")).unwrap();
    let mut framebuf = [0; 160 * 144 * 4];

    'running: loop {
        let frame_start = timer.ticks64();

        for event in events.poll_iter() {
            match event {
                Event::Quit {..} => break 'running,
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
                            Keycode::A => emu.btn_released(joypad::LEFT),
                            Keycode::S => emu.btn_released(joypad::DOWN),
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
        emu.get_debug_framebuf_rgba(&mut framebuf);

        tex.with_lock(None, |pixels, _| {
            pixels.copy_from_slice(&framebuf);
        }).unwrap();
        canvas.copy(&tex, None, None).unwrap();

        canvas.present();

        let frame_duration = timer.ticks64() - frame_start;
        if frame_duration < frame_rate {
            timer.delay((frame_rate - frame_duration) as u32);
        }
    }
}
