document.addEventListener("DOMContentLoaded", function () {
  const tracks = [
    "ambient1.mp3",
    "ambient2.mp3",
    "ambient3.mp3"
  ];

  const bgm = document.getElementById("bgm");
  let currentTrack = 0;

  let lastTab = null;
  let audioUnlocked = false;
  let diaryTimer = null;

  let masterVolume = Number(localStorage.getItem("dnd_master_volume") ?? 0.7);

  const fadeTimers = new WeakMap();

  const volumeSlider =
    document.getElementById("master_volume") ||
    document.querySelector('[id$="-master_volume"]');

  function clampVolume(v) {
    return Math.max(0, Math.min(1, Number(v) || 0));
  }

  function scaledVolume(v) {
    return clampVolume(v * masterVolume);
  }

  function activePaneValue() {
    const pane = document.querySelector(".tab-content > .tab-pane.active");
    if (!pane) return null;
    return pane.getAttribute("data-value");
  }

  function targetBgmVolume(tab) {
    if (tab === "magic") return 0.16;
    if (tab === "blood") return 0.12;
    if (tab === "diary") return 0.30;
    if (tab === "camp") return 0.30;
    return 0.35;
  }

  function stopFade(audioEl) {
    const timer = fadeTimers.get(audioEl);
    if (timer) {
      clearInterval(timer);
      fadeTimers.delete(audioEl);
    }
  }

  function fadeTo(audioEl, targetVolume, duration = 700, pauseWhenDone = false) {
    if (!audioEl) return;

    stopFade(audioEl);

    targetVolume = clampVolume(targetVolume);

    const startVolume = isNaN(audioEl.volume) ? 0 : audioEl.volume;
    const delta = targetVolume - startVolume;

    if (Math.abs(delta) < 0.01) {
      audioEl.volume = targetVolume;

      if (pauseWhenDone && targetVolume === 0) {
        audioEl.pause();
      }

      return;
    }

    const intervalMs = 40;
    const steps = Math.max(1, Math.round(duration / intervalMs));
    let step = 0;

    const timer = setInterval(function () {
      step += 1;

      const progress = step / steps;
      audioEl.volume = clampVolume(startVolume + delta * progress);

      if (step >= steps) {
        clearInterval(timer);
        fadeTimers.delete(audioEl);

        audioEl.volume = targetVolume;

        if (pauseWhenDone && targetVolume === 0) {
          audioEl.pause();
        }
      }
    }, intervalMs);

    fadeTimers.set(audioEl, timer);
  }

  function ensurePlaying(audioEl) {
    if (!audioEl) return;

    audioEl.play().catch(function (err) {
      console.log("Autoplay blocked until user interacts:", err);
    });
  }

  function playTrack(index) {
    if (!bgm) return;

    bgm.src = tracks[index];
    bgm.volume = scaledVolume(targetBgmVolume(activePaneValue()));

    bgm.play().catch(function (err) {
      console.log("BGM autoplay blocked until user interacts:", err);
    });
  }

  function playOneShot(audioEl, volume = 0.2) {
    if (!audioEl) return;

    audioEl.pause();
    audioEl.currentTime = 0;
    audioEl.volume = scaledVolume(volume);

    audioEl.play().catch(function (err) {
      console.log("One-shot audio blocked until user interacts:", err);
    });
  }

  function maybePlayPageTurn(newTab) {
    const page = document.getElementById("page_sfx");

    if (!page) return;
    if (!audioUnlocked) return;
    if (!lastTab || !newTab) return;

    const leavingCamp = lastTab === "camp" && newTab !== "camp";
    const enteringCamp = lastTab !== "camp" && newTab === "camp";

    if (leavingCamp || enteringCamp) {
      playOneShot(page, 0.22);
    }
  }

  function syncBgmVolume() {
    if (!bgm) return;

    const tab = activePaneValue();
    fadeTo(bgm, scaledVolume(targetBgmVolume(tab)), 900, false);
  }

  function syncCampFire() {
    const fire = document.getElementById("fire_sfx");
    const isCamp = activePaneValue() === "camp";

    if (!fire) return;

    if (isCamp) {
      ensurePlaying(fire);
      fadeTo(fire, scaledVolume(0.18), 900, false);
    } else {
      fadeTo(fire, 0.00, 700, true);
    }
  }

  function syncMagicAmbience() {
    const magic = document.getElementById("magic_sfx");
    const isMagic = activePaneValue() === "magic";

    if (!magic) return;

    if (isMagic) {
      ensurePlaying(magic);
      fadeTo(magic, scaledVolume(0.10), 900, false);
    } else {
      fadeTo(magic, 0.00, 700, true);
    }
  }

  function syncBloodAmbience() {
    const blood = document.getElementById("blood_sfx");
    const isBlood = activePaneValue() === "blood";

    if (!blood) return;

    if (isBlood) {
      ensurePlaying(blood);
      fadeTo(blood, scaledVolume(0.10), 900, false);
    } else {
      fadeTo(blood, 0.00, 700, true);
    }
  }

  function scheduleDiarySound() {
    const diary = document.getElementById("diary_sfx");

    if (!diary) return;

    if (diaryTimer) {
      clearTimeout(diaryTimer);
      diaryTimer = null;
    }

    if (!audioUnlocked) return;
    if (activePaneValue() !== "diary") return;

    const delay = 3000 + Math.random() * 4000;

    diaryTimer = setTimeout(function playDiaryOnce() {
      if (activePaneValue() !== "diary") return;

      playOneShot(diary, 0.05);

      const nextDelay = 3000 + Math.random() * 4000;
      diaryTimer = setTimeout(playDiaryOnce, nextDelay);
    }, delay);
  }

  function stopDiarySound() {
    const diary = document.getElementById("diary_sfx");

    if (diaryTimer) {
      clearTimeout(diaryTimer);
      diaryTimer = null;
    }

    if (diary) {
      diary.pause();
      diary.currentTime = 0;
    }
  }

  function syncDiaryAmbience() {
    const isDiary = activePaneValue() === "diary";

    if (isDiary) {
      scheduleDiarySound();
    } else {
      stopDiarySound();
    }
  }

  function syncAllAmbience() {
    syncBgmVolume();
    syncCampFire();
    syncMagicAmbience();
    syncBloodAmbience();
    syncDiaryAmbience();
  }

  function setupVolumeSlider() {
    if (!volumeSlider) return;

    volumeSlider.value = masterVolume;

    function refreshVolumeLabel() {
      const label = document.querySelector('[id$="-master_volume_value"]');
      if (label) label.textContent = Math.round(masterVolume * 100) + "%";
    }

    refreshVolumeLabel();

    volumeSlider.addEventListener("input", function () {
      masterVolume = clampVolume(volumeSlider.value);

      localStorage.setItem("dnd_master_volume", masterVolume);

      refreshVolumeLabel();

      syncAllAmbience();
    });
  }

  if (bgm) {
    bgm.addEventListener("ended", function () {
      currentTrack = (currentTrack + 1) % tracks.length;
      playTrack(currentTrack);
    });

    playTrack(currentTrack);
  }

  function resumeAudioOnce() {
    audioUnlocked = true;

    if (bgm) {
      bgm.play().catch(() => {});
    }

    syncAllAmbience();

    document.removeEventListener("click", resumeAudioOnce);
    document.removeEventListener("keydown", resumeAudioOnce);
  }

  setupVolumeSlider();

  document.addEventListener("click", resumeAudioOnce);
  document.addEventListener("keydown", resumeAudioOnce);

  setTimeout(syncAllAmbience, 150);

  setTimeout(function () {
    lastTab = activePaneValue();
  }, 200);

  $(document).on("shown.bs.tab", 'a[data-toggle="tab"]', function () {
    const newTab = activePaneValue();

    maybePlayPageTurn(newTab);
    syncAllAmbience();

    lastTab = newTab;
  });

  setInterval(syncAllAmbience, 1000);
});
