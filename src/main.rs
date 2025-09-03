use sdl2::event::Event;

fn main() {
    let sdl = sdl2::init().unwrap();
    let video = sdl.video().unwrap();
    let window = video.window("GameboyEmu", 160 * 3, 140 * 3)
        .position_centered()
        .build().unwrap();
    let mut canvas = window.into_canvas()
        .accelerated()
        .target_texture()
        .build().unwrap();
    canvas.set_logical_size(160, 140).unwrap();

    let texture_creator = canvas.texture_creator();

    let mut tex = texture_creator
        .create_texture_streaming(sdl2::pixels::PixelFormatEnum::RGBA32, 160, 140)
        .unwrap();
    tex.set_scale_mode(sdl2::render::ScaleMode::Nearest);

    let mut events = sdl.event_pump().unwrap();
    let timer = sdl.timer().unwrap();
    let frame_rate = (1.0f64 / 59.73 * 1000.0).round() as u64;

    'running: loop {
        let frame_start = timer.ticks64();

        for event in events.poll_iter() {
            match event {
                Event::Quit {..} => break 'running,
                _ => {}
            }
        }

        canvas.set_draw_color(sdl2::pixels::Color::GRAY);
        canvas.clear();

        canvas.present();

        let frame_duration = timer.ticks64() - frame_start;
        if frame_duration < frame_rate {
            timer.delay((frame_rate - frame_duration) as u32);
        }
    }
}
