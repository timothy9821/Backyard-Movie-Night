;; backyard-movie-night.clar
;; Simple neighborhood movie night coordination system

(define-constant contract-owner tx-sender)
(define-constant err-owner-only (err u100))
(define-constant err-event-not-found (err u101))
(define-constant err-already-registered (err u102))
(define-constant err-event-full (err u103))

(define-map events
  { event-id: uint }
  {
    organizer: principal,
    movie-title: (string-ascii 50),
    date: uint,
    max-attendees: uint,
    current-attendees: uint,
    weather-backup: bool,
    backup-location: (string-ascii 100)
  }
)

(define-map attendees
  { event-id: uint, attendee: principal }
  {
    seating-preference: (string-ascii 20),
    snack-assignment: (string-ascii 30),
    equipment-bringing: (string-ascii 50)
  }
)

(define-map equipment-needed
  { event-id: uint, item: (string-ascii 30) }
  { assigned-to: (optional principal), priority: uint }
)

(define-data-var next-event-id uint u1)

(define-public (create-event (movie-title (string-ascii 50)) (date uint) (max-attendees uint) (weather-backup bool) (backup-location (string-ascii 100)))
  (let ((event-id (var-get next-event-id)))
    (map-set events
      { event-id: event-id }
      {
        organizer: tx-sender,
        movie-title: movie-title,
        date: date,
        max-attendees: max-attendees,
        current-attendees: u0,
        weather-backup: weather-backup,
        backup-location: backup-location
      }
    )
    (var-set next-event-id (+ event-id u1))
    (ok event-id)
  )
)

(define-public (register-attendee (event-id uint) (seating-preference (string-ascii 20)) (snack-assignment (string-ascii 30)) (equipment-bringing (string-ascii 50)))
  (let ((event (unwrap! (map-get? events { event-id: event-id }) err-event-not-found)))
    (asserts! (is-none (map-get? attendees { event-id: event-id, attendee: tx-sender })) err-already-registered)
    (asserts! (< (get current-attendees event) (get max-attendees event)) err-event-full)

    (map-set attendees
      { event-id: event-id, attendee: tx-sender }
      {
        seating-preference: seating-preference,
        snack-assignment: snack-assignment,
        equipment-bringing: equipment-bringing
      }
    )

    (map-set events
      { event-id: event-id }
      (merge event { current-attendees: (+ (get current-attendees event) u1) })
    )
    (ok true)
  )
)

(define-public (add-equipment-need (event-id uint) (item (string-ascii 30)) (priority uint))
  (let ((event (unwrap! (map-get? events { event-id: event-id }) err-event-not-found)))
    (asserts! (is-eq tx-sender (get organizer event)) err-owner-only)
    (map-set equipment-needed
      { event-id: event-id, item: item }
      { assigned-to: none, priority: priority }
    )
    (ok true)
  )
)

(define-public (assign-equipment (event-id uint) (item (string-ascii 30)) (assignee principal))
  (let ((event (unwrap! (map-get? events { event-id: event-id }) err-event-not-found))
        (equipment (unwrap! (map-get? equipment-needed { event-id: event-id, item: item }) err-event-not-found)))
    (asserts! (is-eq tx-sender (get organizer event)) err-owner-only)
    (map-set equipment-needed
      { event-id: event-id, item: item }
      (merge equipment { assigned-to: (some assignee) })
    )
    (ok true)
  )
)

(define-read-only (get-event (event-id uint))
  (map-get? events { event-id: event-id })
)

(define-read-only (get-attendee-info (event-id uint) (attendee principal))
  (map-get? attendees { event-id: event-id, attendee: attendee })
)

(define-read-only (get-equipment-assignment (event-id uint) (item (string-ascii 30)))
  (map-get? equipment-needed { event-id: event-id, item: item })
)
